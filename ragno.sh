#./visited_urls.sh  > data/visited_urls.txt 

count=0
errors=0

##  crawl all found domains like www.domain.com and not like www.subdomain.domain.com
./linked-domains.sh | egrep -v "\..+\..+\..+" | egrep "www\." | grep http | fgrep -v -f data/visited_urls.txt | while read url
##  crawl all found domains like domain.com and not like xxxx.domain.com
#./linked-domains.sh | egrep -v "\..+\..+" | egrep ".+\." | grep http | fgrep -v -f data/visited_urls.txt | while read url
do
    echo "Crawling '${url}'..."
    swipl ragno.pl $url 
    if [ $? -eq 0 ] ; then
	echo $url >> data/visited_urls.txt
	errors=0
    else
	echo $url >> data/skipped_urls.txt
 	errors=$(expr $errors + 1)
    fi
    count=$(expr $count + 1)
    sleep 1
    if [ $errors -gt 10 ] ; then
        echo "Too many consecutive errors, quitting!  count=${count}, errors=${errors}"
	exit 1
    fi
done
echo "count=${count}, errors=${errors}"
