local library = import '_library.libsonnet';

{
  resources: [
    // pleroma
    library.git_resource('pleroma', 'https://git.pleroma.social/pleroma/pleroma.git', null, 'stable'),
    library.image_resource('pleroma'),
  ],

  jobs: [
    library.build_push_docker_job('pleroma', 'pleroma', null, 'https://git.pleroma.social/pleroma/pleroma/-/commit/'),
  ],
}
