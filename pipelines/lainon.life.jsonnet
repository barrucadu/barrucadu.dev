local library = import '_library.libsonnet';

local build_deploy_frontend_job =
  {
    name: 'build-deploy-frontend',
    public: true,
    serial: true,
    plan: [
      { get: 'lainonlife-git', trigger: true },
      {
        task: 'build',
        config: library['lainon.life-builder_config'] {
          inputs: [{ name: 'lainonlife-git' }],
          outputs: [{ name: 'site' }],
          run: {
            dir: 'lainonlife-git/frontend',
            path: 'sh',
            args: [
              '-cex',
              |||
                cat <<EOF > config.json
                { "channels":
                  { "everything": { "mpd_host": "localhost", "mpd_port": 6600, "description": "all the music, all the time" }
                  , "cyberia":    { "mpd_host": "localhost", "mpd_port": 6601, "description": "classic lainchan radio: electronic, chiptune, weeb" }
                  , "swing":      { "mpd_host": "localhost", "mpd_port": 6602, "description": "swing, electroswing, and jazz" }
                  , "cafe":       { "mpd_host": "localhost", "mpd_port": 6603, "description": "music to drink tea to" }
                  }

                , "influxdb":
                  { "host": "localhost"
                  , "port": 8086
                  , "user": "root"
                  , "pass": "root"
                  , "db":   "lainon.life"
                  }

                , "template":
                  { "default_channel": "cyberia"
                  , "icecast_status_url": "/radio/status-json.xsl"
                  , "icecast_stream_url_base": "https://lainon.life/radio"
                  , "server_cost": 20.39
                  , "currency_symbol": "€"
                  }
                }
                EOF
                pipenv install
                pipenv run build config.json
                mv _site/* ../../site/
              |||,
            ],
          },
        },
      },
      {
        put: 'frontend-rsync',
        params: {
          path: 'site',
          rsync_args: ['--delete'],
        },
      },
    ],
  };


{
  resource_types: [
    library.resource_type('rsync-resource'),
  ],

  resources: [
    // frontend
    library.git_resource('lainonlife', 'https://github.com/barrucadu/lainonlife.git', paths=['frontend/']),
    library.rsync_resource('frontend', 'lainon.life', '{{lainonlife-ssh-private-key}}', '/srv/http/www'),
    // pleroma
    library.image_resource('pleroma'),
  ],

  jobs: [
    // frontend
    build_deploy_frontend_job,
    // pleroma
    library.deploy_docker_systemd_job('pleroma', 'lainon.life', '{{lainonlife-ssh-private-key}}', null, false),
  ],
}
