#!/usr/bin/env sh
#./visited_urls.sh  > data/visited_urls.txt 

export count=0
export errors=0

export domain=$1

echo extracting Domains to be crawled ..

##  crawl all found domains like www.domain.com and not like www.subdomain.domain.com
##./linked-domains.sh | egrep -v "\..+\..+\..+" | egrep "www\." | grep http | fgrep -v -f data/visited_urls.txt | while read url
##  crawl all found domains like domain.com and not like xxxx.domain.com

#./linked-domains.sh | grep -v porn | grep -v sex | grep -v sex | egrep -v "\..+\..+" | egrep ".+\." | grep https | fgrep -v -f data/visited_urls.txt | fgrep -v -f data/skipped_urls.txt | egrep "/[a-z]" |egrep "${domain}/$" | while read url

./linked-domains.sh | grep -v porn | grep -v sex | grep -v sex | egrep -v "\..+\..+\..+" | grep "/www\." | egrep ".+\." | grep https | fgrep -v -f data/visited_urls.txt | fgrep -v -f data/skipped_urls.txt | egrep "/[a-z]" |egrep "${domain}/$" | while read url

do
    #echo "Crawling '${url}'..."
    swipl ragno.pl $url 
    if [ $? -eq 0 ] ; then
	echo $url >> data/visited_urls.txt
	errors=0
    else
	echo $url >> data/skipped_urls.txt
 	errors=$(expr $errors + 1)
    fi
    count=$(expr $count + 1)
    #sleep 1
    if [ $errors -gt 100 ] ; then
        echo "Too many consecutive errors, quitting!  count=${count}, errors=${errors}"
	exit 1
    fi
done
