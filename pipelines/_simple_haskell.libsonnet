local library = import '_library.libsonnet';

function(package, repo=null, subfolder=null, snapshot_filter_paths=null)
  local repo_base_url = 'https://github.com/barrucadu/' + (if repo == null then package else repo);
  local repo_url = repo_base_url + '.git';
  local commit_url = repo_base_url + '/commit';
  local cabal_file = (if subfolder == null then '' else subfolder + '/') + package + '.cabal';

  local build_test_task(snapshot_build=false, script=null) =
    local default_script = |||
      export LANG=C.UTF-8
      stack="stack --no-terminal"
      if [ -f ../stackage-feed/item ]; then
        apt-get update && apt-get install -y jq
        resolver="$(jq -r .id < ../stackage-feed/item | cut -d/ -f4)"
        $stack init --resolver="$resolver" --force
      fi
      $stack setup
      $stack build
    |||;
    {
      task: 'build-and-test',
      config: {
        platform: 'linux',
        image_resource: {
          type: 'docker-image',
          source: { repository: 'haskell' },
        },
        inputs: [
          { name: 'package-git' },
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
    };

  local deploy_task = {
    task: 'deploy',
    config: {
      platform: 'linux',
      image_resource: {
        type: 'docker-image',
        source: { repository: 'haskell' },
      },
      inputs: [
        { name: 'package-git' },
      ],
      params: {
        PACKAGE: package,
        HACKAGE_USERNAME: 'barrucadu',
        HACKAGE_PASSWORD: '{{hackage-password}}',
        CABAL_FILE: cabal_file,
      },
      run: {
        path: 'sh',
        dir: 'package-git',
        args: [
          '-c',
          |||
            ver=$(grep '^version:' "${CABAL_FILE}" | sed 's/^version: *//')

            if curl -fs "http://hackage.haskell.org/package/${PACKAGE}-${ver}" >/dev/null; then
              echo "version already exists on hackage" >&2
              exit 0
            fi

            stack --no-terminal setup
            echo n | stack --no-terminal upload \
          ||| + if subfolder == null then '.' else subfolder,
        ],
      },
    },
  };

  local test_job(script=null) = {
    name: 'test-' + package,
    plan: [
      { get: 'package-git', resource: package + '-cabal-git', trigger: true },
      build_test_task(false, script),
    ],
  };

  local test_snapshot_job(script=null) = {
    name: 'test-snapshot-' + package,
    plan: [
      { get: 'package-git', resource: package + '-git', trigger: true },
      { get: 'stackage-feed', trigger: true },
      build_test_task(true, script),
    ],
  };

  local deploy_job(script=null) =
    {
      name: 'deploy-' + package,
      plan: [
        { get: 'package-git', resource: package + '-cabal-git', trigger: true, passed: ['test-' + package] },
        if script == null then deploy_task else
          {
            task: 'predeploy-check',
            config: {
              platform: 'linux',
              image_resource: {
                type: 'docker-image',
                source: { repository: 'haskell' },
              },
              inputs: [
                { name: 'package-git' },
              ],
              params: {
                PACKAGE: package,
                CABAL_FILE: cabal_file,
              },
              run: {
                path: 'sh',
                dir: 'package-git',
                args: ['-c', script],
              },
            },
            on_success: deploy_task,
          },
      ],
    };

  local res_cabal = library.git_resource(package + '-cabal', repo_url, [cabal_file]);
  local res_top = library.git_resource(package, repo_url, snapshot_filter_paths);

  {
    resources: {
      all: [res_cabal, res_top],
      cabal: res_cabal,
      top: res_top,
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
