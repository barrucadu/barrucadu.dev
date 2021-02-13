local library = import '_library.libsonnet';

local bad_event =
  {
    put: 'shoggoth-blog-event-api',
    params: {
      phase: 'tag',
      description: 'Internal error, check build log.',
      status: 'Error',
    },
  };

local event(status, phase) =
  {
    put: 'shoggoth-blog-event-api',
    params: {
      path: 'tags',
      status: status,
      phase: phase,
    },
  };

local build_deploy_blog_job =
  {
    name: 'build-deploy-blog',
    public: true,
    serial: true,
    plan: [
      { get: 'blog-git', trigger: true },
      {
        task: 'tag',
        config: library['tag-builder_config'] {
          inputs: [{ name: 'blog-git' }],
          run: {
            path: 'sh',
            args: [
              '-cxe',
              |||
                cd blog-git
                git rev-parse --short HEAD > ../tags/tag
                echo "https://github.com/barrucadu/lookwhattheshoggothdraggedin.com/commit/$(git rev-parse HEAD)" > ../tags/tag_url
                git show -s --format=%s HEAD > ../tags/description
              |||,
            ],
          },
        },
        on_failure: bad_event,
        on_error: bad_event,
      },
      {
        task: 'build',
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
            { name: 'blog-git' },
            { name: 'tags' },
          ],
          outputs: [
            { name: 'site' },
          ],
          run: {
            path: 'sh',
            dir: 'blog-git',
            args: [
              '-cex',
              |||
                pip install -r requirements.txt
                PATH=.:$PATH ./build --out=../site/
              |||,
            ],
          },
        },
        on_success: event('Ok', 'build'),
        on_failure: event('Failure', 'build'),
        on_error: event('Error', 'build'),
      },
      {
        put: 'blog-rsync',
        params: {
          path: 'site',
          rsync_args: ['--delete'],
        },
        on_success: event('Ok', 'deploy'),
        // a failure here indicates an error with the resource config, so
        // class this as an error.
        on_failure: event('Error', 'deploy'),
        on_error: event('Error', 'deploy'),
      },
    ],
  };

{
  resource_types: [
    library.resource_type('event-api-resource'),
    library.resource_type('rsync-resource'),
  ],

  resources: [
    library.git_resource('blog', 'https://github.com/barrucadu/lookwhattheshoggothdraggedin.com.git'),
    library.rsync_resource('blog', 'dunwich.barrucadu.co.uk', '{{dunwich-ssh-private-key}}', '/srv/http/lookwhattheshoggothdraggedin.com/www'),
    library.event_api_resource('shoggoth-blog', '{{event-api-shoggoth-blog-token}}'),
  ],

  jobs: [
    build_deploy_blog_job,
  ],
}
