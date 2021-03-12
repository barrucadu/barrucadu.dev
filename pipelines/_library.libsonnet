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

  // jobs
  build_push_docker_job: function(name, repo, docker_path=null, commit_url=null)
    local dp = name + '-git/' + (if docker_path == null then '' else docker_path + '/');
    {
      name: 'build-' + name,
      public: true,
      plan: [
        { get: name + '-git', trigger: true },
        {
          put: name + '-image',
          params: {
            build: dp,
            dockerfile: dp + 'Dockerfile',
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
