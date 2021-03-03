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

local builder_config(name) =
  {
    platform: 'linux',
    image_resource: {
      type: 'docker-image',
      source: {
        repository: 'registry.barrucadu.dev/' + name + '-builder',
        username: 'registry',
        password: '{{docker-registry-password}}',
      },
    },
  };

local tag_builder_config = builder_config('tag') + {
  outputs: [
    { name: 'tags' },
  ],
};

{
  resource_type: function(name)
    {
      name: name,
      type: 'docker-image',
      source: {
        repository: 'registry.barrucadu.dev/' + name,
        username: 'registry',
        password: '{{docker-registry-password}}',
      },
    },

  // resources

  event_api_resource: function(name, token)
    {
      name: name + '-event-api',
      type: 'event-api-resource',
      source: {
        project: name,
        token: token,
      },
    },

  feed_resource: function(name, uri)
    {
      name: name + '-feed',
      type: 'feed-resource',
      source: {
        uri: uri,
      },
    },

  git_resource: function(name, uri, paths=null, branch=null)
    {
      name: name + '-git',
      type: 'git',
      source: {
        uri: uri,
        [if branch != null then 'branch']: branch,
        [if paths != null then 'paths']: paths,
      },
    },

  image_resource: function(name)
    {
      name: name + '-image',
      type: 'docker-image',
      source: {
        repository: 'registry.barrucadu.dev/' + name,
        username: 'registry',
        password: '{{docker-registry-password}}',
      },
    },

  rsync_resource: function(name, host, key, path)
    {
      name: name + '-rsync',
      type: 'rsync-resource',
      source: {
        server: host,
        private_key: key,
        remote_dir: path,
      },
    },

  // task configs
  'barrucadu.co.uk-builder_config': builder_config('barrucadu.co.uk'),

  'lainon.life-builder_config': builder_config('lainon.life'),

  'tag-builder_config': tag_builder_config,

  // jobs
  build_push_docker_job: function(name, repo, event_resource_name=null, docker_path=null, commit_url=null)
    local ern = if event_resource_name == null then name else event_resource_name;
    local dp = name + '-git/' + (if docker_path == null then '' else docker_path + '/');
    {
      name: 'build-' + name,
      public: true,
      plan: [
        { get: name + '-git', trigger: true },
        {
          task: 'tag',
          config: tag_builder_config {
            inputs: [{ name: name + '-git', path: 'in' }],
            params: {
              COMMIT_URL: if commit_url == null then 'https://github.com/barrucadu/' + repo + '/commit/' else commit_url,
            },
            run: {
              path: 'sh',
              args: [
                '-cxe',
                |||
                  mkdir -p tags/event
                  mkdir -p tags/image
                  cd in
                  #
                  tag_name="$(git rev-parse --short HEAD)"
                  tag_url="${COMMIT_URL}/$(git rev-parse HEAD)"
                  summary="$(git show -s --format=%s HEAD)"
                  #
                  echo $tag_name > ../tags/event/tag
                  echo $tag_url > ../tags/event/tag_url
                  echo $summary > ../tags/event/description
                  #
                  jq -n --arg tag_name "$tag_name" --arg tag_url "$tag_url" --arg summary "$summary" > ../tags/image/labels \
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

  deploy_docker_systemd_job: function(name, host, key, event_resource_name=null, service=null, passed=true)
    local ern = if event_resource_name == null then name else event_resource_name;
    {
      name: 'deploy-' + name,
      public: true,
      serial: true,
      plan: [
        { get: name + '-image', trigger: true, [if passed then 'passed']: ['build-' + name] },
        {
          task: 'tag',
          config: {
            platform: 'linux',
            image_resource: {
              type: 'docker-image',
              source: { repository: 'alpine' },
            },
            inputs: [{ name: name + '-image', path: 'in' }],
            outputs: [{ name: 'tags' }],
            run: {
              path: 'sh',
              args: [
                '-cxe',
                |||
                  apk add --no-cache jq
                  mkdir -p tags/event
                  cd in
                  #
                  cat docker_inspect.json
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
          task: 'deploy',
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
              SERVICE: if service == null then name else service,
              SSH_PRIVATE_KEY: key,
            },
            run: {
              path: 'sh',
              args: [
                '-ce',
                |||
                  echo "$SSH_PRIVATE_KEY" > ssh-private-key
                  chmod 600 ssh-private-key
                  set -x
                  apk add --no-cache openssh
                  ssh -i ssh-private-key -o "StrictHostKeyChecking no" "concourse-deploy-robot@$HOST" sudo systemctl restart "$SERVICE"
                |||,
              ],
            },
          },
          on_success: event(ern, 'Ok', 'deploy'),
          on_failure: event(ern, 'Failure', 'deploy'),
          on_error: event(ern, 'Error', 'deploy'),
        },
      ],
    },
}
