FROM leastauthority/foolscap-base

VOLUME /data

ENV PATH "${PATH}":/app/env/bin

CMD \
    /app/env/bin/flogtool create-gatherer --location "${LAE_GATHERER_HINT}" /data \
    && cd /data \
    && /app/env/bin/twistd --pidfile /var/run/foolscap-gatherer.pid -ny gatherer.tac
