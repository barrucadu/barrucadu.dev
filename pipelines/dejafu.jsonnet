local library = import '_library.libsonnet';

local test_snapshot_job =
{
  name: 'test-snapshot',
  public: true,
  plan: [
    { get: 'dejafu-git', trigger: true, },
    { get: 'stackage-feed', trigger: true, },
    local bad_event = {
      put: 'dejafu-event-api',
      params: {
        phase: 'tag',
        description: 'Internal error, check build log.',
        status: 'Error',
      },
    };
    {
      task: 'Tag',
      config: library['tag-builder_config'] + {
        inputs: [
          { name: 'dejafu-git', },
          { name: 'stackage-feed', },
        ],
        run: {
          path: 'sh',
          args: ['-cxe', |||
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
    local event(status) = {
      put: 'dejafu-event-api',
      params: {
        path: 'tags',
        phase: 'test',
        status: status,
      },
    };
    {
      task: 'Build & Test',
      config: {
        platform: 'linux',
        image_resource: {
          type: 'docker-image',
          source: { repository: 'haskell', },
        },
        inputs: [
          { name: 'dejafu-git', },
          { name: 'tags', },
        ],
        run: {
          path: 'sh',
          dir: 'dejafu-git',
          args: ['-cxe', |||
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
      on_success: event('Ok'),
      on_failure: event('Failure'),
      on_error: event('Error'),
    },
  ],
};

{
  resource_types: [
    library['resource_type']('event-api-resource'),
    library['resource_type']('feed-resource'),
  ],

  resources: [
    library['git_resource']('dejafu', 'https://github.com/barrucadu/dejafu.git', ["dejafu/", "dejafu-tests/"]),
    library['feed_resource']('stackage', 'https://www.stackage.org/feed'),
    library['event_api_resource']('dejafu', '{{event-api-dejafu-token}}'),
  ],

  jobs: [
    test_snapshot_job,
  ],
}
