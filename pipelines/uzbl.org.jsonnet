local library = import '_library.libsonnet';

local copy_git_to_rsync_job(name, repo) =
  {
    name: 'deploy-' + name,
    serial: true,
    plan: [
      { get: name + '-git', trigger: true },
      {
        put: name + '-rsync',
        params: {
          path: name + '-git',
          rsync_args: ['--delete', '--exclude=.git/'],
        },
      },
    ],
  };

{
  resource_types: [
    library.resource_type('rsync-resource'),
  ],

  resources: [
    library.git_resource('website', 'https://github.com/uzbl/uzbl-website.git'),
    library.git_resource('docs', 'https://github.com/uzbl/uzbl.git'),
    library.rsync_resource('website', 'dunwich.barrucadu.co.uk', '((dunwich-ssh-private-key))', '/srv/http/uzbl.org/www'),
    library.rsync_resource('docs', 'dunwich.barrucadu.co.uk', '((dunwich-ssh-private-key))', '/srv/http/uzbl.org/uzbl'),
  ],

  jobs: [
    copy_git_to_rsync_job('website', 'uzbl-website'),
    copy_git_to_rsync_job('docs', 'uzbl'),
  ],
}
