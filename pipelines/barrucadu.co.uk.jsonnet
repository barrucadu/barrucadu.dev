local library = import '_library.libsonnet';

local bad_event(event_resource_name) =
  {
    put: event_resource_name + '-event-api',
    params: {
      phase: 'tag',
      description: 'Internal error, check build log.',
      status: 'Error',
    },
  };

local event(event_resource_name, event_tags_path, status, phase) =
  {
    put: event_resource_name + '-event-api',
    params: {
      path: event_tags_path,
      status: status,
      phase: phase,
    },
  };

local rsync_task(name, event_tags_path) =
  {
    put: name + '-rsync',
    params: {
      path: 'site/' + name,
      rsync_args: ['--delete'],
    },
    on_success: event(name, event_tags_path, 'Ok', 'deploy'),
    // a failure here indicates an error with the resource config, so
    // class this as an error.
    on_failure: event(name, event_tags_path, 'Error', 'deploy'),
    on_error: event(name, event_tags_path, 'Error', 'deploy'),
  };

local run_buildpy_task(name, event_tags_path) =
  {
    config: library['barrucadu.co.uk-builder_config'] {
      inputs: [
        { name: name + '-git' },
        { name: 'tags' },
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
    on_success: event(name, event_tags_path, 'Ok', 'build'),
    on_failure: event(name, event_tags_path, 'Failure', 'build'),
    on_error: event(name, event_tags_path, 'Error', 'build'),
  };

local build_deploy_memo_job =
  {
    name: 'build-deploy-memo',
    public: true,
    serial: true,
    plan: [
      { get: 'memo-git', trigger: true },
      {
        task: 'Tag',
        config: library['tag-builder_config'] {
          inputs: [{ name: 'memo-git' }],
          run: {
            path: 'sh',
            args: [
              '-cxe',
              |||
                cd memo-git
                git rev-parse --short HEAD > ../tags/tag
                echo "https://github.com/barrucadu/memo.barrucadu.co.uk/commit/$(git rev-parse HEAD)" > ../tags/tag_url
                git show -s --format=%s HEAD > ../tags/description
              |||,
            ],
          },
        },
        on_failure: bad_event('memo'),
        on_error: bad_event('memo'),
      },
      { task: 'Build' } + run_buildpy_task('memo', 'tags'),
      rsync_task('memo', 'tags'),
      {
        task: 'Notify',
        params: {
          PLEROMA_PASSWORD: '{{pleroma-user-memo-password}}',
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
            { name: 'tags' },
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
        on_success: event('memo', 'tags', 'Ok', 'notify'),
        on_failure: event('memo', 'tags', 'Error', 'notify'),
        on_error: event('memo', 'tags', 'Error', 'notify'),
      },
    ],
  };

local build_deploy_www_job =
  {
    name: 'build-deploy-www',
    public: true,
    serial: true,
    plan: [
      { get: 'cv-git', trigger: true },
      { get: 'www-git', trigger: true },
      {
        task: 'Tag',
        config: library['tag-builder_config'] {
          inputs: [
            { name: 'cv-git' },
            { name: 'www-git' },
          ],
          run: {
            path: 'sh',
            args: [
              '-cxe',
              |||
                mkdir -p tags/build-cv
                mkdir -p tags/build-site
                mkdir -p tags/deploy
                #
                cd cv-git
                echo "cv:$(git rev-parse --short HEAD)" > ../tags/build-cv/tag
                echo "https://github.com/barrucadu/cv/commit/$(git rev-parse HEAD)" > ../tags/build-cv/tag_url
                git show -s --format=%s HEAD > ../tags/build-cv/description
                #
                cd ../www-git
                echo "site:$(git rev-parse --short HEAD)" > ../tags/build-site/tag
                echo "https://github.com/barrucadu/barrucadu.co.uk/commit/$(git rev-parse HEAD)" > ../tags/build-site/tag_url
                git show -s --format=%s HEAD > ../tags/build-site/description
                #
                cd ..
                echo "$(cat tags/build-cv/tag), $(cat tags/build-site/tag)" > tags/deploy/description
              |||,
            ],
          },
        },
        on_failure: bad_event('www'),
        on_error: bad_event('www'),
      },
      { task: 'Build (site)' } + run_buildpy_task('www', 'tags/build-site'),
      {
        task: 'Build (cv)',
        config: library['barrucadu.co.uk-builder_config'] {
          inputs: [
            { name: 'cv-git' },
            { name: 'site' },
            { name: 'tags' },
          ],
          outputs: [{ name: 'site' }],
          run: {
            dir: 'cv-git',
            path: 'sh',
            args: [
              '-cex',
              |||
                latexmk -pdf -xelatex cv-full.tex
                mv cv-full.pdf ../site/www/cv.pdf
              |||,
            ],
          },
        },
        on_success: event('www', 'tags/build-cv', 'Ok', 'build'),
        on_failure: event('www', 'tags/build-cv', 'Failure', 'build'),
        on_error: event('www', 'tags/build-cv', 'Error', 'build'),
      },
      rsync_task('www', 'tags/deploy'),
    ],
  };

{
  resource_types: [
    library.resource_type('event-api-resource'),
    library.resource_type('rsync-resource'),
  ],

  resources: [
    // bookdb.barrucadu.co.uk
    library.git_resource('bookdb', 'https://github.com/barrucadu/bookdb.git'),
    library.image_resource('bookdb'),
    library.event_api_resource('bookdb', '{{event-api-bookdb-token}}'),
    // memo.barrucadu.co.uk
    library.git_resource('memo', 'https://github.com/barrucadu/memo.barrucadu.co.uk.git'),
    library.rsync_resource('memo', 'dunwich.barrucadu.co.uk', '{{dunwich-ssh-private-key}}', '/srv/http/barrucadu.co.uk/memo'),
    library.event_api_resource('memo', '{{event-api-memo-token}}'),
    // www.barrucadu.co.uk
    library.git_resource('cv', 'https://github.com/barrucadu/cv.git'),
    library.git_resource('www', 'https://github.com/barrucadu/barrucadu.co.uk.git'),
    library.rsync_resource('www', 'dunwich.barrucadu.co.uk', '{{dunwich-ssh-private-key}}', '/srv/http/barrucadu.co.uk/www'),
    library.event_api_resource('www', '{{event-api-www-token}}'),
  ],

  jobs: [
    // websites
    build_deploy_memo_job,
    build_deploy_www_job,
    // bookdb
    library.build_push_docker_job('bookdb', 'bookdb'),
    library.deploy_docker_systemd_job('bookdb', 'dunwich.barrucadu.co.uk', '{{dunwich-ssh-private-key}}'),
  ],
}
