local library = import '_library.libsonnet';

{
  resource_types: [
    library['resource_type']('event-api-resource'),
  ],

  resources: [
    # resources
    library['git_resource']('event-api-resource', 'https://github.com/barrucadu/barrucadu.dev.git',
      paths=['event-api-resource/'],
    ),
    library['image_resource']('event-api-resource'),
    library['git_resource']('rsync-resource', 'https://github.com/barrucadu/barrucadu.dev.git',
      paths=['rsync-resource/'],
    ),
    library['image_resource']('rsync-resource'),
    library['git_resource']('feed-resource', 'https://github.com/barrucadu/barrucadu.dev.git',
      paths=['feed-resource/'],
    ),
    library['image_resource']('feed-resource'),
    # builders
    library['git_resource']('barrucadu.co.uk-builder', 'https://github.com/barrucadu/barrucadu.dev.git',
      paths=['barrucadu.co.uk-builder/'],
    ),
    library['image_resource']('barrucadu.co.uk-builder'),
    library['git_resource']('tag-builder', 'https://github.com/barrucadu/barrucadu.dev.git',
      paths=['tag-builder/'],
    ),
    library['image_resource']('tag-builder'),
    # servers
    library['git_resource']('event-api-server', 'https://github.com/barrucadu/barrucadu.dev.git',
      paths=['event-api-server/'],
    ),
    library['image_resource']('event-api-server'),
    library['git_resource']('frontend', 'https://github.com/barrucadu/barrucadu.dev.git',
      paths=['frontend/'],
    ),
    library['image_resource']('frontend'),
    # events
    library['event_api_resource']('barrucadu.dev', '{{event-api-barrucadu-dev-token}}'),
  ],

  jobs: [
    # resources
    library['build_push_docker_job']('event-api-resource', 'barrucadu.dev', 'barrucadu.dev', 'event-api-resource'),
    library['build_push_docker_job']('rsync-resource', 'barrucadu.dev',
      event_resource_name='barrucadu.dev',
      docker_path='rsync-resource',
    ),
    library['build_push_docker_job']('feed-resource', 'barrucadu.dev',
      event_resource_name='barrucadu.dev',
      docker_path='feed-resource',
    ),
    # builders
    library['build_push_docker_job']('barrucadu.co.uk-builder', 'barrucadu.dev',
      event_resource_name='barrucadu.dev',
      docker_path='barrucadu.co.uk-builder',
    ),
    library['build_push_docker_job']('tag-builder', 'barrucadu.dev',
      event_resource_name='barrucadu.dev',
      docker_path='tag-builder',
    ),
    # servers
    library['build_push_docker_job']('event-api-server', 'barrucadu.dev',
      event_resource_name='barrucadu.dev',
      docker_path='event-api-server',
    ),
    library['deploy_docker_systemd_job']('event-api-server', 'dreamlands.barrucadu.co.uk', '{{dreamlands-ssh-private-key}}',
      event_resource_name='barrucadu.dev',
    ),
    library['build_push_docker_job']('frontend', 'barrucadu.dev',
      event_resource_name='barrucadu.dev',
      docker_path='frontend',
    ),
    library['deploy_docker_systemd_job']('frontend', 'dreamlands.barrucadu.co.uk', '{{dreamlands-ssh-private-key}}',
      event_resource_name='barrucadu.dev',
    ),
  ],
}
