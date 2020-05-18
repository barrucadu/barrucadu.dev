local library = import '_library.libsonnet';

function(package, repo=null, subfolder=null, event_api_resource=null, snapshot_filter_paths=null)
  local repo_base_url = 'https://github.com/barrucadu/' + (if repo == null then package else repo);
  local repo_url = repo_base_url + '.git';
  local commit_url = repo_base_url + '/commit';
  local cabal_file = (if subfolder == null then '' else subfolder + '/') + package + '.cabal';
  local event_api = if event_api_resource == null then package + '-event-api' else event_api_resource;

  local bad_event = {
    put: event_api,
    params: {
      phase: 'tag',
      description: 'Internal error, check build log.',
      status: 'Error',
    },
  };

  local event(phase, status) = {
    put: event_api,
    params: {
      path: 'tags',
      phase: phase,
      status: status,
    },
  };

  local tag_task(snapshot_build=false) =
    local script =
      |||
        git rev-parse --short HEAD > ../tags/tag
        echo "${COMMIT_URL}/$(git rev-parse HEAD)" > ../tags/tag_url
        ver=$(grep '^version:' "${CABAL_FILE}" | sed 's/^version: *//')
        echo "$ver" > ../tags/pkg-ver
      ||| +
      if snapshot_build then
        |||
          jq -r .id < ../stackage-feed/item | cut -d/ -f4 > ../tags/resolver
          echo "Automatic build against new resolver $(cat ../tags/resolver)" > ../tags/description
        |||
      else
        |||
          echo "${PACKAGE}-${ver}" > ../tags/description
        |||;

    {
      task: 'Tag',
      config: library['tag-builder_config'] {
        inputs: [{ name: 'package-git' }] + (if snapshot_build then [{ name: 'stackage-feed' }] else []),
        params: {
          COMMIT_URL: commit_url,
          PACKAGE: package,
          CABAL_FILE: cabal_file,
        },
        run: {
          path: 'sh',
          dir: 'package-git',
          args: ['-cxe', script],
        },
      },
      on_failure: bad_event,
      on_error: bad_event,
    };

  local build_test_task(script=null) =
    local default_script = |||
      export LANG=C.UTF-8
      stack="stack --no-terminal"
      if [ -f ../tags/resolver ]; then
        resolver="$(cat ../tags/resolver)"
        $stack init --resolver="$resolver" --force
      fi
      $stack setup
      $stack build
    |||;
    {
      task: 'Build & Test',
      config: {
        platform: 'linux',
        image_resource: {
          type: 'docker-image',
          source: { repository: 'haskell' },
        },
        inputs: [
          { name: 'package-git' },
          { name: 'tags' },
        ],
        run: {
          path: 'sh',
          dir: 'package-git',
          args: [
            '-cxe',
            if script == null then default_script else script,
          ],
        },
      },
      on_success: event('test', 'Ok'),
      on_failure: event('test', 'Failure'),
      on_error: event('test', 'Error'),
    };

  local deploy_task = {
    task: 'Deploy',
    config: {
      platform: 'linux',
      image_resource: {
        type: 'docker-image',
        source: { repository: 'haskell' },
      },
      inputs: [
        { name: 'package-git' },
        { name: 'tags' },
      ],
      outputs: [
        { name: 'tags' },
      ],
      params: {
        PACKAGE: package,
        HACKAGE_USERNAME: 'barrucadu',
        HACKAGE_PASSWORD: '{{hackage-password}}',
      },
      run: {
        path: 'sh',
        dir: 'package-git',
        args: [
          '-c',
          |||
            ver=$(cat ../tags/pkg-ver)

            if curl -fs "http://hackage.haskell.org/package/${PACKAGE}-${ver}" >/dev/null; then
              echo "version already exists on hackage" >&2
              echo "${PACKAGE}-${ver} (no deploy needed)" > ../tags/description
              exit 0
            fi

            stack --no-terminal setup
            echo n | stack --no-terminal upload \
          ||| + if subfolder == null then '.' else subfolder,
        ],
      },
    },
    on_success: event('deploy', 'Ok'),
    on_failure: event('deploy', 'Failure'),
    on_error: event('deploy', 'Error'),
  };

  local test_job(script=null) = {
    name: 'test-' + package,
    public: true,
    plan: [
      { get: 'package-git', resource: package + '-cabal-git', trigger: true },
      tag_task(false),
      build_test_task(script),
    ],
  };

  local test_snapshot_job(script=null) = {
    name: 'test-snapshot-' + package,
    public: true,
    plan: [
      { get: 'package-git', resource: package + '-git', trigger: true },
      { get: 'stackage-feed', trigger: true },
      tag_task(true),
      build_test_task(script),
    ],
  };

  local deploy_job(script=null) =
    {
      name: 'deploy-' + package,
      public: true,
      plan: [
        { get: 'package-git', resource: package + '-cabal-git', trigger: true, passed: ['test-' + package] },
        tag_task(false),
        if script == null then deploy_task else
          {
            task: 'Predeploy Check',
            config: {
              platform: 'linux',
              image_resource: {
                type: 'docker-image',
                source: { repository: 'haskell' },
              },
              inputs: [
                { name: 'package-git' },
                { name: 'tags' },
              ],
              outputs: [
                { name: 'tags' },
              ],
              params: {
                PACKAGE: package,
              },
              run: {
                path: 'sh',
                dir: 'package-git',
                args: ['-c', script],
              },
            },
            on_success: deploy_task,
            on_failure: event('deploy', 'Failure'),
            on_error: event('deploy', 'Error'),
          },
      ],
    };

  local res_cabal = library.git_resource(package + '-cabal', repo_url, [cabal_file]);
  local res_top = library.git_resource(package, repo_url, snapshot_filter_paths);
  local res_event_api = library.event_api_resource(package, '{{event-api-' + package + '-token}}');

  {
    resources: {
      all: [res_cabal, res_top] + (if event_api_resource == null then [res_event_api] else []),
      cabal: res_cabal,
      top: res_top,
      [if event_api_resource != null then 'event_api']: res_event_api,
    },

    jobs: {
      all: function(test_script=null, deploy_script=null)
        [
          test_job(test_script),
          test_snapshot_job(test_script),
          deploy_job(deploy_script),
        ],

      test: test_job,
      test_snapshot: test_snapshot_job,
      deploy: deploy_job,
    },
  }
