find data/ -name *.json | xargs jq -r '.linkedDomains[]?' | sort -u
