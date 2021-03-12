local library = import '_library.libsonnet';

local build_deploy_blog_job =
  {
    name: 'build-deploy-blog',
    serial: true,
    plan: [
      { get: 'blog-git', trigger: true },
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
      },
      {
        put: 'blog-rsync',
        params: {
          path: 'site',
          rsync_args: ['--delete'],
        },
      },
    ],
  };

{
  resource_types: [
    library.resource_type('rsync-resource'),
  ],

  resources: [
    library.git_resource('blog', 'https://github.com/barrucadu/lookwhattheshoggothdraggedin.com.git'),
    library.rsync_resource('blog', 'dunwich.barrucadu.co.uk', '{{dunwich-ssh-private-key}}', '/srv/http/lookwhattheshoggothdraggedin.com/www'),
  ],

  jobs: [
    build_deploy_blog_job,
  ],
}
