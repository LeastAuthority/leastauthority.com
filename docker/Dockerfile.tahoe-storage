FROM leastauthority/tahoe-base

COPY healthz.rpy /app/code/healthz.rpy

RUN /app/env/bin/tahoe create-node /var/run/storageserver

CMD \
    /app/env/bin/python /app/configure-tahoe /var/run/storageserver < ${CONFIG_PATH} \
    && /app/env/bin/twistd web --port tcp:9001 --resource-script /app/code/healthz.rpy \
    && exec /app/env/bin/tahoe run /var/run/storageserver
