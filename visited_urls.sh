#!/usr/bin/env sh
find data/ -name *.json | xargs jq '.url' | grep http | cut -f2 -d\" | sort -u