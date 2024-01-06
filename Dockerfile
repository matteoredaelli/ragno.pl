FROM swipl

RUN apt-get update && apt-get install -y jq make
RUN rm -rf /var/cache/apt/archives /var/lib/apt/lists/*

COPY . /app

WORKDIR /app

RUN useradd -r -u 1000 -g root appuser
RUN chown -R 1000:root /app
USER appuser

ENV RAGNO_DATA=/app/data

ENTRYPOINT ["make"]
CMD ["make", "run"]