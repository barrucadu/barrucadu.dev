local library = import '_library.libsonnet';

local rsync_task(name) =
  {
    put: name + '-rsync',
    params: {
      path: 'site/' + name,
      rsync_args: ['--delete'],
    },
  };

local run_buildpy_task(name) =
  {
    config: library['barrucadu.co.uk-builder_config'] {
      inputs: [
        { name: name + '-git' },
      ],
      outputs: [
        { name: 'site' },
      ],
      params: {
        OUT_DIR: name,
      },
      run: {
        path: 'sh',
        dir: name + '-git',
        args: [
          '-cex',
          |||
            virtualenv venv
            source venv/bin/activate
            pip install -r requirements.txt
            PATH=.:$PATH ./build --out="../site/${OUT_DIR}"
          |||,
        ],
      },
    },
  };

local build_deploy_memo_job =
  {
    name: 'build-deploy-memo',
    serial: true,
    plan: [
      { get: 'memo-git', trigger: true },
      { task: 'build' } + run_buildpy_task('memo'),
      rsync_task('memo'),
      {
        task: 'notify',
        params: {
          PLEROMA_PASSWORD: '((pleroma-user-memo-password))',
        },
        config: {
          platform: 'linux',
          image_resource: {
            type: 'docker-image',
            source: {
              repository: 'python',
              tag: 3.8,
            },
          },
          inputs: [
            { name: 'memo-git' },
          ],
          run: {
            path: 'sh',
            dir: 'memo-git',
            args: [
              '-cex',
              |||
                pip install -r requirements.txt
                ./post-pleroma-status
              |||,
            ],
          },
        },
      },
    ],
  };

{
  resource_types: [
    library.resource_type('rsync-resource'),
  ],

  resources: [
    // ap.barrucadu.co.uk
    library.image_resource('pleroma'),
    // bookdb.barrucadu.co.uk
    library.git_resource('bookdb', 'https://github.com/barrucadu/bookdb.git'),
    library.image_resource('bookdb'),
    // bookmarks.barrucadu.co.uk
    library.git_resource('bookmarks', 'https://github.com/barrucadu/bookmarks.git'),
    library.image_resource('bookmarks'),
    // memo.barrucadu.co.uk
    library.git_resource('memo', 'https://github.com/barrucadu/memo.barrucadu.co.uk.git'),
    library.rsync_resource('memo', 'dunwich.barrucadu.co.uk', '((dunwich-ssh-private-key))', '/srv/http/barrucadu.co.uk/memo'),
  ],

  jobs: [
    // websites
    build_deploy_memo_job,
    // bookdb
    library.build_push_docker_job('bookdb', 'bookdb'),
    library.deploy_docker_systemd_job('bookdb', 'dunwich.barrucadu.co.uk', '((dunwich-ssh-private-key))'),
    // bookmarks
    library.build_push_docker_job('bookmarks', 'bookmarks'),
    library.deploy_docker_systemd_job('bookmarks', 'dunwich.barrucadu.co.uk', '((dunwich-ssh-private-key))'),
    // pleroma
    library.deploy_docker_systemd_job('pleroma', 'dunwich.barrucadu.co.uk', '((dunwich-ssh-private-key))', null, false),
  ],
}
