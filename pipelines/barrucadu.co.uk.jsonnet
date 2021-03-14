local library = import '_library.libsonnet';

{
  resources: [
    library.git_resource('bookmarks', 'https://github.com/barrucadu/bookmarks.git'),
    library.image_resource('bookmarks'),
  ],

  jobs: [
    library.build_push_docker_job('bookmarks', 'bookmarks'),
    library.deploy_docker_systemd_job('bookmarks', 'dunwich.barrucadu.co.uk', '((dunwich-ssh-private-key))'),
  ],
}
