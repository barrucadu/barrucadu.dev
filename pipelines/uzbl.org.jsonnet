local library = import '_library.libsonnet';

local tag_task(name, repo) =
local bad_event = {
  put: 'uzbl.org-event-api',
  params: {
    phase: 'tag',
    description: 'Internal error, check build log.',
    status: 'Error',
  },
};
{
  task: 'Tag',
  config: library['tag-builder_config'] + {
    inputs: [ { name: name + '-git', } ],
    params: {
      NAME: name,
      REPO: repo,
    },
    run: {
      path: 'sh',
      args: ['-cxe', |||
        cd "${NAME}-git"
        echo "${NAME}:$(git rev-parse --short HEAD)" > ../tags/tag
        echo "https://github.com/uzbl/${REPO}/commit/$(git rev-parse HEAD)" > ../tags/tag_url
        git show -s --format=%s HEAD > ../tags/description
      |||,
      ],
    },
  },
  on_failure: bad_event,
  on_error: bad_event,
};

local copy_git_to_rsync_job(name, repo) =
{
  name: 'deploy-' + name,
  public: true,
  serial: true,
  plan: [
    { get: name + '-git', trigger: true, },
    tag_task(name, repo),
    local event(status) =
    {
      put: 'uzbl.org-event-api',
      params: {
        phase: 'deploy',
        path: 'tags',
        status: status,
      },
    };
    {
      put: name + '-rsync',
      params: {
        path: name + '-git',
        rsync_args: ['--delete', '--exclude=.git/'],
      },
      on_success: event('Ok'),
      on_failure: event('Failure'),
      on_error: event('Error'),
    }
  ],
};

{
  resource_types: [
    library['resource_type']('event-api-resource'),
    library['resource_type']('rsync-resource'),
  ],

  resources: [
    library['git_resource']('website', 'https://github.com/uzbl/uzbl-website.git'),
    library['git_resource']('docs', 'https://github.com/uzbl/uzbl.git'),
    library['rsync_resource']('website', 'dunwich.barrucadu.co.uk', '{{dunwich-ssh-private-key}}', '/srv/http/uzbl.org/www'),
    library['rsync_resource']('docs', 'dunwich.barrucadu.co.uk', '{{dunwich-ssh-private-key}}', '/srv/http/uzbl.org/docs'),
    library['event_api_resource']('uzbl.org', '{{event-api-uzbl-org-token}}'),
  ],

  jobs: [
    copy_git_to_rsync_job('website', 'uzbl-website'),
    copy_git_to_rsync_job('docs', 'uzbl'),
  ],
}
