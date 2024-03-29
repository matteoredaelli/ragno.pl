RAGNO_DATA ?= ~/Downloads/ragnopl

visited_domains.csv:
	find ${RAGNO_DATA} -name *.json | xargs jq -r '.domain, .finalDomain' | sort -u > ${RAGNO_DATA}/$@

linked_domains.csv:
	find ${RAGNO_DATA} -name *.json | xargs jq -r '.linkedDomains[]?' | sort -u > ${RAGNO_DATA}/$@

new_domains.csv: linked_domains.csv visited_domains.csv
## [X] considering only https: converting http links to https links
## [X] removing adult contents
## [X] removing www?.  (www2,..)
	cat ${RAGNO_DATA}/linked_domains.csv | grep -v porn | grep -v sex | grep -v xxx | sed -e 's/http:/https:/' | sed -r 's/www[0-9]?\.//'| egrep "^https?://(www\.)?[^.]+\.[^.]+$$" | fgrep -v -f ${RAGNO_DATA}/visited_domains.csv | fgrep -v -f ${RAGNO_DATA}/skipped_urls.csv > ${RAGNO_DATA}/$@

run: new_domains.csv
	RAGNO_DATA=${RAGNO_DATA} bin/ragno.sh ${NEW_DOMAIN}

