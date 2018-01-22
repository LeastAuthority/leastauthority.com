from __future__ import unicode_literals, print_function

from time import time
from datetime import timedelta
from glob import glob
from os import environ
from os.path import getmtime

from twisted.web.resource import Resource

# The maximum distance into the past which will be considered "recent".  If a
# state file hasn't been updated within this amount of time, something is
# considered to have gone wrong.
RECENT_INTERVAL = timedelta(seconds=60 * 60)


class Healthz(Resource):
    """
    A health-reporting resource.

    This aggregates the results of a number of other health probes into a
    single result exposed over HTTP.

    :ivar list healthz: The health probes being aggregated.
    """
    def __init__(self, healthz):
        Resource.__init__(self)
        self.healthz = healthz


    def render_GET(self, request):
        """
        Serve a 200 response if everything is healthy, 500 otherwise.
        """
        if all(health.probe() for health in self.healthz):
            return b""
        request.setResponseCode(500)
        return b""



class _ConfigHealth(object):
    """
    Monitor a config file for up-to-date-ness.

    If the config file has a different modification time than it did when this
    object was created, it is considered unhealthy.  This facilitates restarts
    on config updates.

    :ivar bytes _config_path: The path to the file to monitor.
    :ivar float _config_mtime: The modification time of the file at the point
        this object was created.
    """
    def __init__(self, config_path):
        self._config_path = config_path
        self._config_mtime = getmtime(self._config_path)


    def probe(self):
        """
        Check the modification time and report whether it has changed.

        :return bool: ``True`` if the modification time is the same, ``False``
            otherwise.
        """
        return self._config_mtime == getmtime(self._config_path)



class _StateHealth(object):
    """
    Monitor a state file for stale-ness.

    If the state file hasn't changed in "a while" it is considered unhealthy.
    This allows us to notice when a process which should be running
    periodically has ceased to (without knowing how or why).  This helps
    handle unknown faults in the system.

    :ivar bytes _state_path: The path to the file to monitor.
    """
    def __init__(self, state_path):
        self._state_path = state_path


    def probe(self):
        """
        Check the modification time and report whether it is "recent".

        :return bool: ``True`` if the modification time is "recent", ``False``
            otherwise.
        """
        return time() - getmtime(self._state_path) < RECENT_INTERVAL.total_seconds()


# Determine the paths we're meant to monitor.
config_path = environ["CONFIG_PATH"]
state_paths = environ["STATE_PATHS"]

# Create the resource (to be served with `twistd web --resource-script ...`).
healthz = Healthz(
    [_ConfigHealth(config_path)] + list(
        _StateHealth(path)
        for path
        # Allow globs in this one because we happen to have two of them to
        # monitor as this is being written.
        in glob(state_paths)
    ),
)

resource = Resource()
resource.putChild(b"", healthz)
