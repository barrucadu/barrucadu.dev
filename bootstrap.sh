#!/bin/sh

set -ex

cat ~/secrets/nixos/dreamlands/registry-password.txt | docker login --username registry --password-stdin https://registry.barrucadu.dev

cd ../tag-builder
docker build -t registry.barrucadu.dev/tag-builder:latest .
docker push registry.barrucadu.dev/tag-builder:latest
