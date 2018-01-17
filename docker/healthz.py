from __future__ import unicode_literals, print_function

from time import time
from glob import glob
from os import environ
from os.path import getmtime

from twisted.web.resource import Resource


class Healthz(Resource):
    def __init__(self, supervisord, healthz):
        Resource.__init__(self)
        self.supervisord = supervisord
        self.healthz = healthz


    def render_GET(self, request):
        if all(health.probe() for health in self.healthz):
            return b""
        request.setResponseCode(500)
        return b""



class _ConfigHealth(object):
    def __init__(self, config_path):
        self._config_path = config_path
        self._config_mtime = getmtime(self._config_path)


    def probe(self):
        return self._config_mtime == getmtime(self._config_path)



class _StateHealth(object):
    def __init__(self, state_path):
        self._state_path = state_path

    def probe(self):
        return time() - getmtime(self._state_path) < 60 * 60



config_path = environ["CONFIG_PATH"]
state_paths = environ["STATE_PATHS"]
resource = Healthz(
    [_ConfigHealth(config_path)] + list(
        _StateHealth(path)
        for path
        in glob.glob(state_paths)
    ),
)
