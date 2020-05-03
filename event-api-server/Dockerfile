FROM haskell:8.6.5

RUN apt-get update && apt-get install -y libpq-dev && rm -rf /var/lib/apt/lists/*

RUN useradd -m app
COPY --chown=app . /app
WORKDIR /app
USER app

RUN stack build --copy-bins && rm -rf .stack-work && rm -rf ~/.stack && cp -a /home/app/.local/bin/ /app

ENTRYPOINT ["/app/bin/event-api-server"]
CMD ["run"]
