{
  resource_type: function(name)
    {
      name: name,
      type: 'docker-image',
      source: {
        repository: 'registry.barrucadu.dev/' + name,
        username: 'registry',
        password: '((docker-registry-password))',
      },
    },

  // resources

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
        password: '((docker-registry-password))',
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

  // jobs
  build_push_docker_job: function(name, repo, docker_path=null, commit_url=null)
    local dp = name + '-git/' + (if docker_path == null then '' else docker_path + '/');
    {
      name: 'build-' + name,
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
}
