
import logging, pprint, sys
from urllib import quote
from cgi import escape as htmlEscape

from twisted.web.resource import Resource


class HandlerBase(Resource):
    def __init__(self, out=None, *a, **kw):
        Resource.__init__(self, *a, **kw)
        if out is None:
            out = sys.stdout
        self.out = out

    def _logger_helper(self, definitionmodulename):
        long_name = "%s.%s" % (definitionmodulename, self.__class__.__name__)
        self._log = logging.getLogger(long_name)
        self._log.debug('Initialized.')

    def render_POST(self, request):
        self.log_request(request)
        return self.render(self, request)

    def render_GET(self, request):
        self.log_request(request)
        return self.render(self, request)

    def log_request(self, request):
        details = dict(
            [ (k, getattr(request, k))
              for k in ['method',
                        'uri',
                        'path',
                        'args',
                        'received_headers']
              ])
        details['client-ip'] = request.getClientIP()
        self._log.debug('Request details from %r:\n%s', request, pprint.pformat(details))

    def get_arg(self, request, argname):
        try:
            [arg] = request.args[argname]
        except (KeyError, ValueError):
            arg = ""

        # Quote the arguments to avoid injection attacks when we interpolate them into HTML
        # attributes (enclosed in "") and CSV values. We could use htmlEscape, but that does
        # not escape ',' or newlines so we would need additional quoting for CSV.
        # URL-encoding with the set of safe-characters below will work for all these cases.
        # Note that the safe set must include characters that are valid in email addresses.
        return quote(arg, safe=' !#$()*+-./:=?@^_`{|}~')


class RequestOutputStream(object):
    def __init__(self, request, tee=None):
        self.request = request
        self.tee = tee

    def write(self, s):
        # reject non-shortest-form encodings, which might defeat the escaping
        s.decode('utf-8', 'strict')
        self.request.write(htmlEscape(s))
        if self.tee:
            self.tee.write(s)

    def writelines(self, seq):
        for s in seq:
            self.write(s)

    def flush(self):
        pass

    def isatty(self):
        return False

    def close(self):
        pass
