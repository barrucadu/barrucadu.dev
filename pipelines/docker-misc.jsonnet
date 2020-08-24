local library = import '_library.libsonnet';

{
  resource_types: [
    library.resource_type('event-api-resource'),
  ],

  resources: [
    // pleroma
    library.git_resource('pleroma', 'https://git.pleroma.social/pleroma/pleroma.git', null, 'stable'),
    library.image_resource('pleroma'),
    library.event_api_resource('pleroma', '{{event-api-pleroma-token}}'),
  ],

  jobs: [
    library.build_push_docker_job('pleroma', 'pleroma', null, null, 'https://git.pleroma.social/pleroma/pleroma/-/commit/'),
  ],
}
