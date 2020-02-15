FROM haskell:8.6.5

RUN apt-get update && apt-get install -y libpq-dev && rm -rf /var/lib/apt/lists/*

RUN useradd -m eventapi
COPY --chown=eventapi . /app
WORKDIR /app
USER eventapi

RUN stack build --copy-bins && rm -rf .stack-work && rm -rf ~/.stack

CMD ["/home/eventapi/.local/bin/event-api-server", "run"]
