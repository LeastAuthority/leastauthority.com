#!/bin/bash -ex

TAHOE_ENV=$1
shift

INTRODUCER=$1
shift

PROBE_FILE=$(mktemp /tmp/probe-XXXXXXXXXXXX)
dd if=/dev/urandom of="${PROBE_FILE}" count=256 bs=1024

TAHOE_NODE=$(mktemp -d /tmp/tahoe-lafs-node-XXXXXXXXXXXX)

TAHOE="${TAHOE_ENV}/bin/tahoe"
CFG="${TAHOE_NODE}/tahoe.cfg"

if [ ! -e "${TAHOE}" ]; then
    virtualenv --python=python2 "${TAHOE_ENV}"
    "${TAHOE_ENV}"/bin/pip install tahoe-lafs
fi

"${TAHOE}" \
    create-client \
    --introducer "${INTRODUCER}" \
    --shares-needed=1 \
    --shares-happy=1 \
    --shares-total=1 \
    "${TAHOE_NODE}"
sed --regexp-extended --in-place=.bak -e 's/web.port = tcp:3456:/web.port = tcp:0:/' "${CFG}"
"${TAHOE}" start "${TAHOE_NODE}"

# It might not be up yet.  Give it a moment and then give it a few chances.
SLEEP_TIME=5
sleep ${SLEEP_TIME}
while :; do
    CAP=$("${TAHOE}" -d "${TAHOE_NODE}" put "${PROBE_FILE}" || true)
    if [ "${CAP}" != "" ]; then
	break
    fi
    # systemd-resolved has a very aggressive cache.  Blow it away.
    sudo systemd-resolve --flush-caches || true
    sleep ${SLEEP_TIME}
    NEW_TIME=$((${SLEEP_TIME} + 1))
    if [ ${NEW_TIME} -le 15 ]; then
	SLEEP_TIME=${NEW_TIME}
    fi
done

diff "${PROBE_FILE}" <("${TAHOE}" -d "${TAHOE_NODE}" get "${CAP}")
[ $? -eq 0 ] && echo "Success" || echo "Failure"
"${TAHOE}" stop "${TAHOE_NODE}"

rm -r "${PROBE_FILE}" "${TAHOE_NODE}"
