rmdir data/
docker rmi ragnopl
docker build -t ragnopl .
mkdir data/
docker run --rm -it -v data:/app/data ragnopl bash
