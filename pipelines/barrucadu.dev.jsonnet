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
    // builders
    library.git_resource(
      'barrucadu.co.uk-builder',
      'https://github.com/barrucadu/barrucadu.dev.git',
      paths=['barrucadu.co.uk-builder/'],
    ),
    library.image_resource('barrucadu.co.uk-builder'),
    library.git_resource(
      'lainon.life-builder',
      'https://github.com/barrucadu/barrucadu.dev.git',
      paths=['lainon.life-builder/'],
    ),
    library.image_resource('lainon.life-builder'),
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
    // builders
    library.build_push_docker_job(
      'barrucadu.co.uk-builder',
      'barrucadu.dev',
      docker_path='barrucadu.co.uk-builder',
    ),
    library.build_push_docker_job(
      'lainon.life-builder',
      'barrucadu.dev',
      docker_path='lainon.life-builder',
    ),
  ],
}
