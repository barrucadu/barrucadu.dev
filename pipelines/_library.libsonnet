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
  build_push_docker_job: function(name, repo, docker_path=null, commit_url=null)
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
                  cd in
                  #
                  tag_name="$(git rev-parse --short HEAD)"
                  tag_url="${COMMIT_URL}/$(git rev-parse HEAD)"
                  summary="$(git show -s --format=%s HEAD)"
                  jq -n --arg tag_name "$tag_name" --arg tag_url "$tag_url" --arg summary "$summary" > ../tags/labels \
                     '{ "barrucadu.tag.name": $tag_name, "barrucadu.tag.url": $tag_url, "barrucadu.summary": $summary}'
                |||,
              ],
            },
          },
        },
        {
          put: name + '-image',
          params: {
            build: dp,
            dockerfile: dp + 'Dockerfile',
            labels_file: 'tags/labels',
            tag_as_latest: true,
          },
        },
      ],
    },

  deploy_docker_systemd_job: function(name, host, key, service=null, passed=true)
    {
      name: 'deploy-' + name,
      public: true,
      serial: true,
      plan: [
        { get: name + '-image', trigger: true, [if passed then 'passed']: ['build-' + name] },
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
        },
      ],
    },
}
