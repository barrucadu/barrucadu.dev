local library = import '_library.libsonnet';

local deploy_pleroma_to_host = function(host, key)
{
  name: 'deploy-' + host,
  serial: true,
  plan: [
    {
      get: 'pleroma-image',
      trigger: true,
      passed: ['build-pleroma'],
    },
    {
      task: 'deploy',
      config: {
        platform: 'linux',
        image_resource: {
          type: 'docker-image',
          source: {
            repository: 'alpine',
          },
        },
        params: {
          HOST: host,
          SSH_PRIVATE_KEY: key,
        },
        run: {
          path: 'sh',
          args: [
            '-ce',
            |||
              echo "$SSH_PRIVATE_KEY" > ssh-private-key
              chmod 600 ssh-private-key
              set -x
              apk add --no-cache openssh
              ssh -i ssh-private-key -o "StrictHostKeyChecking no" "concourse-deploy-robot@$HOST" sudo systemctl restart pleroma
            |||,
          ],
        },
      },
    },
  ],
};

{
  resources: [
    // pleroma
    library.git_resource('pleroma', 'https://git.pleroma.social/pleroma/pleroma.git', null, 'stable'),
    library.image_resource('pleroma'),
  ],

  jobs: [
    library.build_push_docker_job('pleroma', 'pleroma', null, 'https://git.pleroma.social/pleroma/pleroma/-/commit/'),
    deploy_pleroma_to_host('dunwich.barrucadu.co.uk', '((dunwich-ssh-private-key))'),
    deploy_pleroma_to_host('social.lainon.life', '((lainonlife-ssh-private-key))'),
  ],
}
