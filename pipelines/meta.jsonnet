local library = import '_library.libsonnet';

{
  resources: [
    // resources
    library.git_resource(
      'rsync-resource',
      'https://github.com/barrucadu/barrucadu.dev.git',
      paths=['rsync-resource/'],
    ),
    library.image_resource('rsync-resource'),
    library.git_resource(
      'feed-resource',
      'https://github.com/barrucadu/barrucadu.dev.git',
      paths=['feed-resource/'],
    ),
    library.image_resource('feed-resource'),
  ],

  jobs: [
    // resources
    library.build_push_docker_job(
      'rsync-resource',
      'barrucadu.dev',
      docker_path='rsync-resource',
    ),
    library.build_push_docker_job(
      'feed-resource',
      'barrucadu.dev',
      docker_path='feed-resource',
    ),
  ],
}
