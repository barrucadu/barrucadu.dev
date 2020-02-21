local bad_event(event_resource_name) = {
  put: event_resource_name + '-event-api',
  params: {
    phase: 'tag',
    description: 'Internal error, check build log.',
    status: 'Error',
  },
};

local event(event_resource_name, status, phase) =
{
  put: event_resource_name + '-event-api',
  params: {
    phase: phase,
    path: 'tags/event',
    status: status,
  },
};

local tag_builder_config =
{
  platform: 'linux',
  image_resource: {
    type: 'docker-image',
    source: {
      repository: 'registry.barrucadu.dev/tag-builder',
      username: 'registry',
      password: '{{docker-registry-password}}',
    },
  },
  outputs: [
    { name: 'tags' }
  ],
};

{
  'resource_type': function(name)
  {
    name: name,
    type: 'docker-image',
    source: {
      repository: 'registry.barrucadu.dev/' + name,
      username: 'registry',
      password: '{{docker-registry-password}}',
    },
  },

  # resources

  'event_api_resource': function(name, token)
  {
    name: name + '-event-api',
    type: 'event-api-resource',
    source: {
      project: name,
      token: token,
    }
  },

  'feed_resource': function(name, uri)
  {
    name: name + '-feed',
    type: 'feed-resource',
    source: {
      uri: uri,
    },
  },

  'git_resource': function(name, uri, paths=null)
  {
    name: name + '-git',
    type: 'git',
    source: {
      uri: uri,
      [if paths != null then 'paths']: paths,
    }
  },

  'image_resource': function(name)
  {
    name: name + '-image',
    type: 'docker-image',
    source: {
      repository: 'registry.barrucadu.dev/' + name,
      username: 'registry',
      password: '{{docker-registry-password}}',
    },
  },

  'rsync_resource': function(name, host, key, path)
  {
    name: name + '-rsync',
    type: 'rsync-resource',
    source: {
      server: host,
      private_key: key,
      remote_dir: path,
    },
  },

  # task configs
  'barrucadu.co.uk-builder_config':
    {
      platform: 'linux',
      image_resource: {
        type: 'docker-image',
        source: {
          repository: 'registry.barrucadu.dev/barrucadu.co.uk-builder',
          username: 'registry',
          password: '{{docker-registry-password}}',
        },
      },
    },

  'tag-builder_config': tag_builder_config,

  # jobs
  'build_push_docker_job': function(name, repo, event_resource_name=null, docker_path=null)
  local ern = if event_resource_name == null then name else event_resource_name;
  local dp = name + '-git/' + (if docker_path == null then '' else docker_path + '/');
  {
    name: 'build-' + name,
    public: true,
    plan: [
      { get: name + '-git', trigger: true, },
      {
        task: 'Tag',
        config: tag_builder_config + {
          inputs: [ { name: name + '-git', path: 'in' } ],
          params: {
            REPO: repo,
          },
          run: {
            path: 'sh',
            args: ['-cxe', |||
              mkdir -p tags/event
              mkdir -p tags/image
              cd in
              #
              git rev-parse --short HEAD > ../tags/event/tag
              echo "https://github.com/barrucadu/${REPO}/commit/$(git rev-parse HEAD)" > ../tags/event/tag_url
              git show -s --format=%s HEAD > ../tags/event/description
              #
              jq -n --arg tag_name "$(tag_name)" --arg tag_url "$(tag_url)" --arg summary "$(summary)" > ../tags/image/labels \
                 '{ "barrucadu.tag.name": $tag_name, "barrucadu.tag.url": $tag_url, "barrucadu.summary": $summary}'
            |||,
            ],
          },
        },
        on_failure: bad_event(ern),
        on_error: bad_event(ern),
      },
      {
        put: name + '-image',
        params: {
          build: dp,
          dockerfile: dp + 'Dockerfile',
          labels_file: 'tags/image/labels',
          tag_as_latest: true,
        },
        on_success: event(ern, 'Ok', 'build'),
        on_failure: event(ern, 'Failure', 'build'),
        on_error: event(ern, 'Error', 'build'),
      },
    ],
  },

  'deploy_docker_systemd_job': function(name, host, key, event_resource_name=null)
  local ern = if event_resource_name == null then name else event_resource_name;
  {
    name: 'deploy-' + name,
    public: true,
    serial: true,
    plan: [
      { get: name + '-image', trigger: true, passed: ['build-' + name] },
      {
        task: 'Tag',
        config: {
          platform: 'linux',
          image_resource: {
            type: 'docker-image',
            source: { repository: 'alpine' },
          },
          inputs: [ { name: name + '-image', path: 'in' } ],
          outputs: [ { name: 'tags' } ],
          run: {
            path: 'sh',
            args: ['-cxe', |||
              apk add --no-cache jq
              mkdir -p tags/event
              cd in
              #
              jq -r '.[0].Config.Labels["barrucadu.tag.name"]' < docker_inspect.json > ../tags/event/tag
              jq -r '.[0].Config.Labels["barrucadu.tag.url"]' < docker_inspect.json > ../tags/event/tag_url
              jq -r '.[0].Config.Labels["barrucadu.summary"]' < docker_inspect.json > ../tags/event/description
            |||,
            ],
          },
        },
        on_failure: bad_event(ern),
        on_error: bad_event(ern),
      },
      {
        task: 'Deploy',
        config: {
          platform: 'linux',
          image_resource: {
            type: 'docker-image',
            source: {
              repository: 'alpine',
            },
          },
          params: {
            HOST: host,
            SERVICE: name,
            SSH_PRIVATE_KEY: key,
          },
          run: {
            path: 'sh',
            args: ['-ce', |||
              echo "$SSH_PRIVATE_KEY" > ssh-private-key
              chmod 600 ssh-private-key
              set -x
              apk add --no-cache openssh
              ssh -i ssh-private-key -o "StrictHostKeyChecking no" "concourse-deploy-robot@$HOST" sudo systemctl restart "$SERVICE"
            |||
            ],
          },
        },
        on_success: event(ern, 'Ok', 'deploy'),
        on_failure: event(ern, 'Failure', 'deploy'),
        on_error: event(ern, 'Error', 'deploy'),
      }
    ],
  },
}
