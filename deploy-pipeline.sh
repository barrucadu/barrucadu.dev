#!/bin/sh

NAME="$1"
YAML="pipelines/${NAME}.yaml"
SECRETS="$HOME/secrets/concourse/params-dreamlands.yml"

if [[ ! -f "$YAML" ]]; then
  echo "file ${YAML} not found."
  exit 1
fi

if [[ ! -f "$SECRETS" ]]; then
  echo "file ${SECRETS} not found."
  exit 1
fi

fly set-pipeline -t dreamlands -p "$NAME" -c "$YAML" -l "$SECRETS"
