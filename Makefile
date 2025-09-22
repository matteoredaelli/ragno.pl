RAGNO_DATA ?= data

${RAGNO_DATA}/domains.csv:
	rocksdb_ldb  scan --db=ragnodb/ --max_keys=10 |sed -e s'/.*==> /DUMMYDUMMY/' | tr '\n' ' ' | sed -e 's/DUMMYDUMMY/\n/g' | jq -r '.domains[]' | sed -e 's/^www\d*\.//' | sort -u > $@

${RAGNO_DATA}/visited.csv:
	rocksdb_ldb  scan --db=ragnodb/ --max_keys=10 |sed -e s'/.*==> /DUMMYDUMMY/' | tr '\n' ' ' | sed -e 's/DUMMYDUMMY/\n/g' | jq -r '.domain' | sort -u | grep -v "^$$" > $@

${RAGNO_DATA}/new_domains.csv: ${RAGNO_DATA}/domains.csv ${RAGNO_DATA}/visited.csv
	grep -v -f $$RAGNO_DATA/visited.csv $$RAGNO_DATA/domains.csv | grep -v -f $$RAGNO_DATA/skipped.csv >$@

clean:
	rm -f ${RAGNO_DATA}/new_domains.csv ${RAGNO_DATA}/domains.csv ${RAGNO_DATA}/visited.csv

run: ${RAGNO_DATA}/new_domains.csv
	RAGNO_DATA=${RAGNO_DATA} bin/ragno.sh ${NEW_DOMAIN}
	rm -f  ${RAGNO_DATA}/new_domains.csv ${RAGNO_DATA}/domains.csv ${RAGNO_DATA}/visited.csv

lint:
	swipl -q --on-warning=status --on-error=status -g check -t halt -l ragno.pl

tests:
	swipl -g "set_prolog_flag(test, true)" -t "run_tests" tests.pl
