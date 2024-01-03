FROM swipl

RUN apt-get update && apt-get install -y jq
RUN rm -rf /var/cache/apt/archives /var/lib/apt/lists/*

COPY . /app

WORKDIR /app

CMD ["bash", "/app/bin/ragno.sh"]