#!/usr/bin/env sh
find data/ -name *.json | xargs jq '.linked_domains' | grep http | cut -f2 -d\" | sort -u
