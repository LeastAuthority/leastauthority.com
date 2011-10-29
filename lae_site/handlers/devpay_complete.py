import logging
import pprint
import time

from twisted.web.resource import Resource
from lae_site.util.timestamp import format_iso_time


class DevPayPurchaseHandler (Resource):
    def __init__(self, *a, **kw):
        Resource.__init__(self, *a, **kw)
        self._log = logging.getLogger(self.__class__.__name__)
        self._log.debug('Initialized.')

    def render_POST(self, request):
        return self.render_GET(self, request)

    def render_GET(self, request):
        try:
            [activationkey] = request.args['ActivationKey']
        except (KeyError, ValueError):
            activationkey = "unknown"

        try:
            [productcode] = request.args['ProductCode']
        except (KeyError, ValueError):
            productcode = "unknown"

        else:
            return self._render_activate(request, activationkey, productcode)

    def _render_activate(self, request, activationkey, productcode):
        f = open("activation_requests", "a+")
        try:
            f.write("%s %s %s\n" % (format_iso_time(time.time()), activationkey, productcode))
        finally:
            f.close()
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
        request.setResponseCode(307)
        request.setHeader('Location', '/products/%s.html' % (productcode,))
        return ""
