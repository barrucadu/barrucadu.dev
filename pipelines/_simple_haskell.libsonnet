local library = import '_library.libsonnet';

function(package)
  local bad_event = {
    put: package + '-event-api',
    params: {
      phase: 'tag',
      description: 'Internal error, check build log.',
      status: 'Error',
    },
  };

  local event(phase, status) = {
    put: package + '-event-api',
    params: {
      path: 'tags',
      phase: phase,
      status: status,
    },
  };

  local build_test_task = {
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
          |||
            export LANG=C.UTF-8
            stack="stack --no-terminal"
            if [ -f ../tags/resolver ]; then
              resolver="$(cat ../tags/resolver)"
              $stack init --resolver="$resolver" --force
            fi
            $stack setup
            $stack build
          |||,
        ],
      },
    },
    on_success: event('test', 'Ok'),
    on_failure: event('test', 'Failure'),
    on_error: event('test', 'Error'),
  };

  local test_snapshot_job =
    {
      name: 'test-snapshot-' + package,
      public: true,
      plan: [
        { get: 'package-git', resource: package + '-git', trigger: true },
        { get: 'stackage-feed', trigger: true },
        {
          task: 'Tag',
          config: library['tag-builder_config'] {
            inputs: [
              { name: 'package-git' },
              { name: 'stackage-feed' },
            ],
            params: {
              PACKAGE: package,
            },
            run: {
              path: 'sh',
              dir: 'package-git',
              args: [
                '-cxe',
                |||
                  git rev-parse --short HEAD > ../tags/tag
                  echo "https://github.com/barrucadu/${PACKAGE}/commit/$(git rev-parse HEAD)" > ../tags/tag_url
                  jq -r .id < ../stackage-feed/item | cut -d/ -f4 > ../tags/resolver
                  echo "Automatic build against new resolver $(cat ../tags/resolver)" > ../tags/description
                |||,
              ],
            },
          },
          on_failure: bad_event,
          on_error: bad_event,
        },
        build_test_task,
      ],
    };

  local test_job =
    {
      name: 'test-' + package,
      public: true,
      plan: [
        { get: 'package-git', resource: package + '-cabal-git', trigger: true },
        {
          task: 'Tag',
          config: library['tag-builder_config'] {
            inputs: [
              { name: 'package-git' },
            ],
            params: {
              PACKAGE: package,
            },
            run: {
              path: 'sh',
              dir: 'package-git',
              args: [
                '-cxe',
                |||
                  git rev-parse --short HEAD > ../tags/tag
                  echo "https://github.com/barrucadu/${PACKAGE}/commit/$(git rev-parse HEAD)" > ../tags/tag_url
                  ver=$(grep '^version:' "${PACKAGE}.cabal" | sed 's/^version: *//')
                  echo "${PACKAGE}-${ver}" > ../tags/description
                |||,
              ],
            },
          },
          on_failure: bad_event,
          on_error: bad_event,
        },
        build_test_task,
      ],
    };

  local deploy_job =
    {
      name: 'deploy-' + package,
      public: true,
      plan: [
        { get: 'package-git', resource: package + '-cabal-git', trigger: true, passed: ['test-' + package] },
        {
          task: 'Tag',
          config: library['tag-builder_config'] {
            inputs: [
              { name: 'package-git' },
            ],
            params: {
              PACKAGE: package,
            },
            run: {
              path: 'sh',
              dir: 'package-git',
              args: [
                '-cxe',
                |||
                  git rev-parse --short HEAD > ../tags/tag
                  echo "https://github.com/barrucadu/${PACKAGE}/commit/$(git rev-parse HEAD)" > ../tags/tag_url
                  ver=$(grep '^version:' "${PACKAGE}.cabal" | sed 's/^version: *//')
                  echo "$ver" > ../tags/pkg-ver
                  echo "${PACKAGE}-${ver}" > ../tags/description
                |||,
              ],
            },
          },
          on_failure: bad_event,
          on_error: bad_event,
        },
        {
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
                  echo n | stack --no-terminal upload .
                |||,
              ],
            },
          },
          on_success: event('deploy', 'Ok'),
          on_failure: event('deploy', 'Failure'),
          on_error: event('deploy', 'Error'),
        },
      ],
    };

  {
    resource_types: [
      library.resource_type('event-api-resource'),
      library.resource_type('feed-resource'),
    ],

    resources: [
      library.git_resource(package + '-cabal', 'https://github.com/barrucadu/' + package + '.git', [package + '.cabal']),
      library.git_resource(package, 'https://github.com/barrucadu/' + package + '.git'),
      library.event_api_resource(package, '{{event-api-' + package + '-token}}'),
    ],

    jobs: [
      test_job,
      deploy_job,
      test_snapshot_job,
    ],
  }
