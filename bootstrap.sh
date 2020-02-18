#!/bin/sh

set -ex

cat ~/secrets/nixos/dreamlands/registry-password.txt | docker login --username registry --password-stdin https://registry.barrucadu.dev

cd event-api-resource
docker build -t registry.barrucadu.dev/event-api-resource:latest .
docker push registry.barrucadu.dev/event-api-resource:latest

cd ../event-api-server
docker build -t registry.barrucadu.dev/event-api-server:latest .
docker push registry.barrucadu.dev/event-api-server:latest
