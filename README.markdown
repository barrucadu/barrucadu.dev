barrucadu.dev
=============

This repository contains everything but the NixOS config for my dev
server.  The NixOS config is in the [nixfiles][] repository.

[nixfiles]: https://github.com/barrucadu/nixfiles

*-resource
----------

Docker images defining Concourse [resource types][]:

- `feed-resource`: read-only access to an atom or RSS feed.

    **Source parameters:**

    - `uri`

- `rsync-resource`: read/write access to another filesystem over `rsync`.

    **Source parameters:**

    - `server`
    - `user` (default `concourse-deploy-robot`)
    - `remote_dir`
    - `private_key`
    - `port` (default `22`)

    **Put parameters:**

    - `rsync_args` (default `[]`): extra arguments to pass to rsync
    - `path` (default `""`): path to copy from

[resource types]: https://concourse-ci.org/resource-types.html


pipelines
---------

Concourse piplines, in jsonnet format.
