local library = import '_library.libsonnet';

local bad_event = {
  put: 'dejafu-event-api',
  params: {
    phase: 'tag',
    description: 'Internal error, check build log.',
    status: 'Error',
  },
};

local event(phase, status) = {
  put: 'dejafu-event-api',
  params: {
    path: 'tags',
    phase: phase,
    status: status,
  },
};

local test_snapshot_job =
  {
    name: 'test-snapshot',
    public: true,
    plan: [
      { get: 'dejafu-git', trigger: true },
      { get: 'concurrency-cabal-git', trigger: true },
      { get: 'dejafu-cabal-git', trigger: true },
      { get: 'hunit-dejafu-cabal-git', trigger: true },
      { get: 'tasty-dejafu-cabal-git', trigger: true },
      { get: 'stackage-feed', trigger: true },
      {
        task: 'Tag',
        config: library['tag-builder_config'] {
          inputs: [
            { name: 'dejafu-git' },
            { name: 'stackage-feed' },
          ],
          run: {
            path: 'sh',
            args: [
              '-cxe',
              |||
                cd dejafu-git
                git rev-parse --short HEAD > ../tags/tag
                echo "https://github.com/barrucadu/dejafu/commit/$(git rev-parse HEAD)" > ../tags/tag_url
                #
                jq -r .id < ../stackage-feed/item | cut -d/ -f4 > ../tags/resolver
                echo "Automatic build against new resolver $(cat ../tags/resolver)" > ../tags/description
              |||,
            ],
          },
        },
        on_failure: bad_event,
        on_error: bad_event,
      },
      {
        task: 'Build & Test',
        config: {
          platform: 'linux',
          image_resource: {
            type: 'docker-image',
            source: { repository: 'haskell' },
          },
          inputs: [
            { name: 'dejafu-git' },
            { name: 'tags' },
          ],
          run: {
            path: 'sh',
            dir: 'dejafu-git',
            args: [
              '-cxe',
              |||
                resolver=$(cat ../tags/resolver)
                stack="stack --no-terminal"

                # don't build all the dejafu-bench dependencies to speed up compilation
                # stack's docs suggest this should work: `stack build dejafu-tests:dejafu-tests`
                # but it doesn't, it still builds dejafu-bench too
                sed -n '/executable dejafu-bench/q;p' dejafu-tests/dejafu-tests.cabal > dejafu-tests.cabal
                mv dejafu-tests.cabal dejafu-tests/dejafu-tests.cabal

                # use a utf-8 locale so hedgehog failure output doesn't
                # cause an encoding error - this was the default in the
                # haskell:8.8.1 image but not after that.
                export LANG=C.UTF-8

                $stack init --resolver=$resolver --force
                $stack setup
                $stack build dejafu-tests
                $stack exec dejafu-tests
              |||,
            ],
          },
        },
        on_success: event('test', 'Ok'),
        on_failure: event('test', 'Failure'),
        on_error: event('test', 'Error'),
      },
    ],
  };

local deploy_job(package) =
  {
    name: 'deploy-' + package,
    public: true,
    plan: [
      { get: 'dejafu-git', resource: package + '-cabal-git', trigger: true, passed: ['test-snapshot'] },
      {
        task: 'Tag',
        config: library['tag-builder_config'] {
          inputs: [
            { name: 'dejafu-git' },
          ],
          params: {
            PACKAGE: package,
          },
          run: {
            path: 'sh',
            args: [
              '-cxe',
              |||
                cd dejafu-git
                git rev-parse --short HEAD > ../tags/tag
                echo "https://github.com/barrucadu/dejafu/commit/$(git rev-parse HEAD)" > ../tags/tag_url
              |||,
            ],
          },
        },
        on_failure: bad_event,
        on_error: bad_event,
      },
      {
        task: 'Predeploy Check',
        config: {
          platform: 'linux',
          image_resource: {
            type: 'docker-image',
            source: { repository: 'haskell' },
          },
          inputs: [
            { name: 'dejafu-git' },
            { name: 'tags' },
          ],
          outputs: [
            { name: 'tags' },
          ],
          params: {
            PACKAGE: package,
            HACKAGE_PASSWORD: '{{hackage-password}}',
          },
          run: {
            path: 'sh',
            dir: 'dejafu-git',
            args: [
              '-c',
              |||
                ver=`grep '^version:' "${PACKAGE}/${PACKAGE}.cabal" | sed 's/^version: *//'`
                echo "${PACKAGE}-${ver}" > ../tags/description

                if curl -fs "http://hackage.haskell.org/package/${PACKAGE}-${ver}" >/dev/null; then
                  echo "version already exists on hackage"
                  echo "${PACKAGE}-${ver} (no deploy needed)" > ../tags/description
                  exit 0
                fi

                fail=false
                if ! grep -q -E "tag: *${PACKAGE}-${ver}" "${PACKAGE}/${PACKAGE}.cabal"; then
                  echo "missing tag in ${PACKAGE}/${PACKAGE}.cabal" >&2
                  fail=true
                fi
                if ! grep -q "^${ver}" "${PACKAGE}/CHANGELOG.rst"; then
                  echo "missing header in ${PACKAGE}/CHANGELOG.rst" >&2
                  fail=true
                fi
                if ! grep -q -E "Git.*${PACKAGE}-${ver}" "${PACKAGE}/CHANGELOG.rst"; then
                  echo "missing tag in ${PACKAGE}/CHANGELOG.rst" >&2
                  fail=true
                fi
                if ! grep -q -E "Hackage.*${PACKAGE}-${ver}" "${PACKAGE}/CHANGELOG.rst"; then
                  echo "missing hackage link in ${PACKAGE}/CHANGELOG.rst" >&2
                  fail=true
                fi
                if ! grep -q -E "${PACKAGE}.*${ver}" README.markdown; then
                  echo "missing package version in README.markdown" >&2
                  fail=true
                fi
                if ! grep -q -E "${PACKAGE}.*${ver}" doc/getting_started.rst; then
                  echo "missing package version in doc/getting_started.rst" >&2
                  fail=true
                fi
                if git grep -q unreleased "$PACKAGE"; then
                  echo "'unreleased' appears in source" >&2
                  fail=true
                fi
                if $fail; then
                  exit 1
                fi

                stack --no-terminal setup
                echo -e "barrucadu\n${HACKAGE_PASSWORD}\nn" | stack --no-terminal upload "$PACKAGE"
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
    library.git_resource('dejafu', 'https://github.com/barrucadu/dejafu.git', ['dejafu/', 'dejafu-tests/']),
    library.git_resource('concurrency-cabal', 'https://github.com/barrucadu/dejafu.git', ['concurrency/concurrency.cabal']),
    library.git_resource('dejafu-cabal', 'https://github.com/barrucadu/dejafu.git', ['dejafu/dejafu.cabal']),
    library.git_resource('hunit-dejafu-cabal', 'https://github.com/barrucadu/dejafu.git', ['hunit-dejafu/hunit-dejafu.cabal']),
    library.git_resource('tasty-dejafu-cabal', 'https://github.com/barrucadu/dejafu.git', ['tasty-dejafu/tasty-dejafu.cabal']),
    library.feed_resource('stackage', 'https://www.stackage.org/feed'),
    library.event_api_resource('dejafu', '{{event-api-dejafu-token}}'),
  ],

  jobs: [
    test_snapshot_job,
    deploy_job('concurrency'),
    deploy_job('dejafu'),
    deploy_job('hunit-dejafu'),
    deploy_job('tasty-dejafu'),
  ],
}
