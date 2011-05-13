import logging
import pprint

from twisted.web.resource import Resource
from twisted.web import http


class DevPayPurchaseHandler (Resource):
    def __init__(self, *a, **kw):
        Resource.__init__(self, *a, **kw)
        self._log = logging.getLogger(self.__class__.__name__)
        self._log.debug('Initialized.')

    def render_POST(self, request):
        return self._render_bad_request(request)

    def render_GET(self, request):
        try:
            [activationkey] = request.args['ActivationKey']
            [productcode] = request.args['ProductCode']
        except (KeyError, ValueError):
            return self._render_bad_request(request)

        else:
            return self._render_activate(request, activationkey, productcode)

    @staticmethod
    def _render_bad_request(request):
        request.setResponseCode(http.BAD_REQUEST)
        request.setHeader('content-type', 'text/ascii')
        return 'Invalid request - POST not supported.'

    def _render_activate(self, request, activationkey, productcode):
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
        request.setResponseCode(200)
        request.setHeader('content-type', 'text/ascii')
        return pprint.pformat(details)
