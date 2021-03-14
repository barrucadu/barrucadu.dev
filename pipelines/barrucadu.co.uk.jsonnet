local library = import '_library.libsonnet';

{
  resources: [
    // ap.barrucadu.co.uk
    library.image_resource('pleroma'),
    // bookdb.barrucadu.co.uk
    library.git_resource('bookdb', 'https://github.com/barrucadu/bookdb.git'),
    library.image_resource('bookdb'),
    // bookmarks.barrucadu.co.uk
    library.git_resource('bookmarks', 'https://github.com/barrucadu/bookmarks.git'),
    library.image_resource('bookmarks'),
  ],

  jobs: [
    // bookdb
    library.build_push_docker_job('bookdb', 'bookdb'),
    library.deploy_docker_systemd_job('bookdb', 'dunwich.barrucadu.co.uk', '((dunwich-ssh-private-key))'),
    // bookmarks
    library.build_push_docker_job('bookmarks', 'bookmarks'),
    library.deploy_docker_systemd_job('bookmarks', 'dunwich.barrucadu.co.uk', '((dunwich-ssh-private-key))'),
    // pleroma
    library.deploy_docker_systemd_job('pleroma', 'dunwich.barrucadu.co.uk', '((dunwich-ssh-private-key))', null, false),
  ],
}
