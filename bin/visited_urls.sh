#!/usr/bin/env sh
find data/ -name *.json | xargs jq -r '.domain, .finalDomain' | sort -u
