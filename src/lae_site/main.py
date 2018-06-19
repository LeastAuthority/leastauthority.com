# before importing Twisted
import mimetypes
mimetypes.add_type("text/plain", ".rst")

if __name__ == '__main__':
    from sys import argv
    from twisted.internet.task import react
    from lae_site.main import main

    react(main, argv[1:])

import sys
import logging
from io import BytesIO
from base64 import b64encode

from twisted.python.log import startLogging, err, msg
from twisted.python.url import URL
from twisted.internet.endpoints import serverFromString
from twisted.application.internet import StreamServerEndpointService
from twisted.application.service import MultiService
from twisted.internet.defer import Deferred
from twisted.python.usage import UsageError, Options
from twisted.python.filepath import FilePath
from twisted.python.components import proxyForInterface
from twisted.web.resource import Resource
from twisted.web.server import NOT_DONE_YET
from twisted.web.http_headers import Headers
from twisted.web.client import (
    FileBodyProducer,
    IAgent,
    Agent,
    readBody,
)

from wormhole import wormhole

from lae_util import opt_metrics_port
from lae_util.eliot_destination import (
    opt_eliot_destination,
    eliot_logging_service,
)

from lae_site.handlers import make_resource, make_site, make_redirector_site
from lae_site.handlers.create_subscription import ChargeBee, Mailer

from lae_automation.signup import (
    provision_subscription,
    get_provisioner,
    get_email_signup,
    get_wormhole_signup,
)
from lae_automation.confirmation import (
    send_signup_confirmation, send_notify_failure,
)

root_log = logging.getLogger(__name__)

def urlFromBytes(b):
    return URL.fromText(b.decode("utf-8"))


@opt_metrics_port
class SiteOptions(Options):
    optFlags = [
        # TODO:
        # Make this HTTP-only.
        # Terminate TLS externally.
        # On K8S on AWS, consider using
        # http://kubernetes.io/docs/user-guide/services/#ssl-support-on-aws
    ]

    optParameters = [
        ("stripe-publishable-api-key-path", None, None, "A path to a file containing a publishable Stripe API key.", FilePath),

        ("chargebee-secret-api-key-path", None, None, "A path to a file containing a ChargeBee API key.", FilePath),
        ("chargebee-site-name", None, None, "The name of the ChargeBee site owning the API key."),
        ("chargebee-plan-id", None, None,
         "The identifier of a ChargeBee plan to associate with new subscriptions.",
        ),
        ("chargebee-gateway-account-id", None, None,
         "The ChargeBee payment gateway through which payment has been processed.",
        ),

        ("site-logs-path", None, None, "A path to a file to which HTTP logs for the site will be written.", FilePath),
        ("wormhole-result-path", None, None,
         "A path to a file to which wormhole interaction results will be written.",
         FilePath,
        ),

        ("redirect-to-port", None, None, "A TCP port number to which to redirect for the TLS site.", int),
        ("subscription-manager", None, None, "Base URL of the subscription manager API.",
         urlFromBytes,
        ),
        ("rendezvous-url", None, URL.fromText(u"ws://wormhole:4000/v1"),
         "The URL of the Wormhole Rendezvous server for wormhole-based signup.",
         urlFromBytes,
        ),
        ("cross-domain", None, None, "The domain for allowing cross origin for the subscription form"
            "(useful for different environment switching)",
        ),
        ("signup-failure-address", None, None,
         "The email address to which to send notification of signup errors.",
        )
    ]

    def __init__(self, reactor):
        Options.__init__(self)
        self.reactor = reactor
        self["secure-ports"] = []
        self["insecure-ports"] = []


    opt_eliot_destination = opt_eliot_destination


    def _parse_endpoint(self, label, description):
        """
        Parse a Twisted endpoint description string into an endpoint or
        convert the parse error into a raised L{UsageError}.
        """
        try:
            return serverFromString(self.reactor, description)
        except Exception as e:
            raise UsageError(
                u"Could not parse {label} value {description}: {error}".format(
                    label=label,
                    description=description,
                    error=str(e),
                    )
                )


    def opt_secure_port(self, endpoint_description):
        """
        A Twisted endpoint description string describing an address at
        which to listen for secure web client connections.  The
        website will be served here.  This option must be used at
        least once.
        """
        endpoint = self._parse_endpoint(u"secure-port", endpoint_description)
        self["secure-ports"].append(endpoint)


    def opt_insecure_port(self, endpoint_description):
        """
        A Twisted endpoint description string describing an address at
        which to listen for insecure web client connections.  A
        redirect will be returned sending the client to a secure
        location where the website can be accessed.  This option may
        be used zero or more times.
        """
        endpoint = self._parse_endpoint(u"insecure-port", endpoint_description)
        self["insecure-ports"].append(endpoint)


    def postOptions(self):
        required_options = [
            "stripe-publishable-api-key-path",
            "chargebee-secret-api-key-path",
            "chargebee-site-name",
            "chargebee-plan-id",
            "chargebee-gateway-account-id",
            "subscription-manager",
            "site-logs-path",
            "wormhole-result-path",
            "cross-domain"
        ]
        for option in required_options:
            if self[option] is None:
                raise UsageError("Missing required option --{}".format(option))

        if not self["secure-ports"]:
            raise UsageError(
                u"Use --secure-port at least once to specify an address for "
                u"the website."
            )
        if self["redirect-to-port"] is not None and not self["insecure-ports"]:
            raise UsageError(
                u"Use --insecure-port at least once or there is no server to "
                u"use --redirect-to-port value."
            )

        p = self["site-logs-path"].parent()
        if not p.isdir():
            p.makedirs()



def main(reactor, *argv):
    o = SiteOptions(reactor)
    try:
        o.parseOptions(argv)
    except UsageError as e:
        raise SystemExit(str(e))

    eliot_logging_service(
        reactor,
        o.get("destinations", []),
    ).startService()

    logging.basicConfig(
        stream = sys.stdout,
        level = logging.DEBUG,
        format = '%(asctime)s %(levelname) 7s [%(name)-65s L%(lineno)d] %(message)s',
        datefmt = '%Y-%m-%dT%H:%M:%S%z',
        )

    startLogging(sys.stdout, setStdout=False)

    metrics = o.get_metrics_service(reactor)
    metrics.privilegedStartService()
    metrics.startService()

    d = Deferred()
    d.callback(None)
    d.addCallback(
        lambda ignored: start_site(
            reactor,
            site_for_options(reactor, o),
            o["secure-ports"],
            o["insecure-ports"],
            o["redirect-to-port"],
        ),
    )
    d.addCallback(lambda ignored: Deferred())
    return d



def site_for_options(reactor, options):
    provisioner = get_provisioner(
        reactor,
        options["subscription-manager"],
        provision_subscription,
    )

    def get_signup(style):
        if style == u"wormhole":
            return get_wormhole_signup(
                reactor,
                provisioner,
                wormhole,
                options["rendezvous-url"],
                options["wormhole-result-path"],
            )
        elif style == u"email":
            return get_email_signup(
                reactor,
                provisioner,
                send_signup_confirmation,
                send_notify_failure,
            )
        else:
            raise ValueError(
                "Don't know about signup configuration {}".format(
                    options["signup"],
                ),
            )

    chargebee_secret_key = options[
        "chargebee-secret-api-key-path"
    ].getContent().strip()
    resource = make_resource(
        options["stripe-publishable-api-key-path"].getContent().strip(),
        options["chargebee-plan-id"],
        get_signup,
        ChargeBee(
            chargebee_secret_key,
            options["chargebee-site-name"],
            options["chargebee-gateway-account-id"],
        ),
        Mailer(
            'support@leastauthority.com',
            options["signup-failure-address"]
            if options["signup-failure-address"] is not None
            else "support-staging@leastauthority.com"
            if "www-staging" in options["cross-domain"]
            else "support@leastauthority.com"
        ),
        options["cross-domain"],
    )

    # Expose some ChargeBee APIs, too.  These cannot be queried by a browser
    # directly so we proxy them here.
    resource.putChild(
        "chargebee",
        create_chargebee_resources(
            reactor,
            options["chargebee-site-name"],
            chargebee_secret_key,
        ),
    )

    site = make_site(resource, options["site-logs-path"])
    return site



def create_chargebee_resources(reactor, site_name, secret_key):
    chargebee = Resource()
    estimates = Resource()
    estimates.putChild(
        "create_subscription",
        ChargebeeCreateSubscription(
            Agent(reactor),
            site_name,
            secret_key,
        ),
    )
    chargebee.putChild("estimates", estimates)
    return chargebee


class AuthenticatingAgent(proxyForInterface(IAgent, "_agent")):
    def __init__(self, agent, auth_header):
        super(AuthenticatingAgent, self).__init__(agent)
        self._auth_header = auth_header


    def request(self, method, uri, headers=None, bodyProducer=None):
        if headers is None:
            headers = Headers()
        headers.addRawHeader(*self._auth_header)
        return super(AuthenticatingAgent, self).request(
            method,
            uri,
            headers,
            bodyProducer,
        )


class ChargebeeCreateSubscription(Resource):
    def __init__(self, agent, site_name, secret_key):
        authorization = b64encode(secret_key + ":")
        self._agent = AuthenticatingAgent(
            agent,
            ("Authorization", "Basic {}".format(authorization)),
        )
        self._site_name = site_name
        self._uri = URL(
            u"https",
            self._site_name + u".chargebee.com",
            [u"api", u"v2", u"estimates", u"create_subscription"],
        )
        msg("Proxying to {}".format(self._uri))


    def render_POST(self, request):
        headers = request.requestHeaders.copy()
        headers.setRawHeaders("Host", [self._uri.host])
        body = FileBodyProducer(BytesIO(request.content.read()))
        d = self._agent.request(
            "POST",
            self._uri.to_uri().to_text().encode("ascii"),
            headers,
            body,
        )
        d.addCallback(self._proxy_response, request)
        return NOT_DONE_YET

    def _proxy_response(self, response, request):
        request.responseHeaders = response.headers
        request.setResponseCode(response.code, response.phrase)
        d = readBody(response)
        d.addCallback(request.write)
        d.addErrback(err, "proxying estimates/create_subscription")
        d.addCallback(lambda ign: request.finish())
        return d


def start_site(reactor, site, secure_ports, insecure_ports, redirect_to_port):
    parent = MultiService()
    for secure in secure_ports:
        StreamServerEndpointService(secure, site).setServiceParent(parent)

    if insecure_ports:
        redirector = make_redirector_site(redirect_to_port)
        for insecure in insecure_ports:
            StreamServerEndpointService(insecure, redirector).setServiceParent(parent)

    parent.privilegedStartService()
    parent.startService()
