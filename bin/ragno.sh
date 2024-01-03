#!/usr/bin/env sh

touch data/visited_urls.csv 
touch data/skipped_urls.csv 

export count=0
export errors=0
export domain=$1

STOPFILE=data/stop.txt

echo extracting Domains to be crawled ..

##  crawl all found domains like www.domain.com and not like www.subdomain.domain.com
##./linked-domains.sh | egrep -v "\..+\..+\..+" | egrep "www\." | grep http | fgrep -v -f data/visited_urls.txt | while read url
##  crawl all found domains like domain.com and not like xxxx.domain.com

#./linked-domains.sh | grep -v porn | grep -v sex | grep -v sex | egrep  "^https?://(www\.)?[^.]+\.[^.]+$"  | fgrep -v -f data/visited_urls.txt | fgrep -v -f data/skipped_urls.txt | egrep "/[a-z]" |egrep "${domain}/$" | while read url

bin/visited_urls.sh > data/visited_urls.csv

if [ -z $domain ] ; then
    bin/linked_domains.sh | grep -v porn | grep -v sex | grep -v sex | egrep  "^https?://(www\.)?[^.]+\.[^.]+$"  | fgrep -v -f data/visited_urls.csv | fgrep -v -f data/skipped_urls.csv  > data/linked_domains.csv
else
    echo $domain >> data/linked_domains.csv
fi


cat data/linked_domains.csv | while read url
do
    if [ -f $STOPFILE ] ; then
       echo "Stopping because I found the file $STOPFILE . Remove it if you want to go on crawling"
       exit 100
    fi

    echo "Crawling '${url}'..."
    swipl ragno.pl "${url}"
    if [ $? -eq 0 ] ; then
	errors=0
    else
	echo $url >> data/skipped_urls.csv
 	errors=$(expr $errors + 1)
    fi
    count=$(expr $count + 1)
    #sleep 1
    if [ $errors -gt 1 ] ; then
        echo "Too many consecutive errors, quitting!  count=${count}, errors=${errors}"
	exit 1
    fi
done
