#!/usr/bin/env sh
echo Visited Urls
wc -l data/visited_urls.txt 
echo "tobe visited https domains (with one dot):"
./linked-domains.sh | egrep -v "\..+\..+" | egrep ".+\." | grep https | fgrep -v -f data/visited_urls.txt | wc -l
echo "tobe visited https domains (with www and two dots):"
./linked-domains.sh | egrep -v "\..+\..+\..+" | egrep "www\." | grep https | fgrep -v -f data/visited_urls.txt | wc -l
echo "tobe visited https domains (with two dots):"
./linked-domains.sh | egrep -v "\..+\..+\..+" | grep https | fgrep -v -f data/visited_urls.txt | wc -l

