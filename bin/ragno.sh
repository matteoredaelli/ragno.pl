#!/usr/bin/env bash

export count=0
export errors=0
export domain=$1

if [ -z $RAGNO_DATA ] ; then
    echo Missing env RAGNO_DATA
    exit 100
fi


STOPFILE=$RAGNO_DATA/stop.txt

MAXERRORS=5


cat $RAGNO_DATA/new_domains.csv | while read url
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
	echo $url >> $RAGNO_DATA/skipped_urls.csv
 	errors=$(expr $errors + 1)
    fi
    count=$(expr $count + 1)
    #sleep 1
    if [ $errors -gt  $MAXERRORS ] ; then
        echo "Too many consecutive errors, quitting!  count=${count}, errors=${errors}"
	exit 1
    fi
done
