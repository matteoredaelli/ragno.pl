if [ -z $RAGNO_DATA ] ; then
    RAGNO_DATA=~/Downloads/ragnopl
fi

docker rmi ragnopl
docker build -t ragnopl .
#docker run --rm -it -v $RAGNO_DATA:/app/data ragnopl bin/ragno.sh
