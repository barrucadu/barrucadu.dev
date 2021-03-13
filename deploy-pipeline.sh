#!/usr/bin/env nix-shell
#! nix-shell -i bash -p jsonnet

set -eo pipefail

NAME="$1"
JSONNET="pipelines/${NAME}.jsonnet"
JSON="pipelines/${NAME}.json"

if [[ ! -f "$JSONNET" ]]; then
  echo "file ${JSONNET} not found."
  exit 1
fi

# `fly` throws a syntax error if your interpolated vars are quoted...
jsonnet "$JSONNET" | sed 's/"((/((/g' | sed 's/))"/))/g' > "$JSON"
fly set-pipeline -t dreamlands -p "$NAME" -c "$JSON"
