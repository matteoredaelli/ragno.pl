FROM swipl

RUN apt-get update && apt-get install -y jq
RUN rm -rf /var/cache/apt/archives /var/lib/apt/lists/*

COPY . /app

WORKDIR /app

ENV RAGNO_DATA=/app/data

CMD ["bash", "/app/bin/ragno.sh"]