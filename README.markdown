barrucadu.dev
=============

This repository contains everything but the NixOS config for my dev
server.  The NixOS config is in the [nixfiles][] repository.

[nixfiles]: https://github.com/barrucadu/nixfiles

*-builder
---------

Docker images used to run Concourse [tasks][] in:

- `barrucadu.co.uk-builder`: building my website, which needs TeXlive and a few other things.
- `tag-builder`: running "tag" tasks to generate details for the `event-api-resource`.  Needs git.

[tasks]: https://concourse-ci.org/tasks.html


*-resource
----------

Docker images defining Concourse [resource types][]:

- `event-api-resource`: read/write access to the `event-api-server`.

    **Source parameters:**

    - `uri` (default `https://event-api.barrucadu.dev`): API server base URI
    - `project`: event group to use
    - `token`: JWT access token (for writing)

    **Put parameters:**

    - `path` (optional): path to read parameters from
    - `status` (one of `Ok`, `Failure`, `Error`)
    - `description`
    - `phase` (optional): arbitrary text providing extra context (like "build" or "deploy")
    - `show_link` (default `true`): link to the Concourse job
    - `link` (optional): URL to use instead of to the Concourse job
    - `tag` (optional, can only be set via `path`): git tag (or similar) identifying the build
    - `tag_url` (optional, can only be set via `path`): link to the `tag`

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


event-api-server
----------------

Repository of "events": currently just Concourse build statuses.

**Usage:**

Run `event-api-server` to get a list of commands.

**Endpoints:**

- `GET /events?count=:count` (`count` defaults to 150 is not set)
- `GET /event/:uuid`
- `GET /projects`
- `GET /project/:name`
- `GET /project/:name/events?count=:count&since=:uuid` (`count` defaults to 150 is not set)
- `POST /project/:name/event` (requires auth)

**Authentication:**

Send a header `Authorization: Bearer {token}` with any POST requests.

Bearer tokens are scoped to projects.


frontend
--------

Renders the frontend [www.barrucadu.dev][].

[www.barrucadu.dev]: https://www.barrucadu.dev/


pipelines
---------

Concourse piplines, in jsonnet format.
