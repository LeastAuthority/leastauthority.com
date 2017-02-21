"""
This module implements an HTTP-accessible microservice which
provides persistence of S4 subscriptions.
"""

from io import BytesIO
from json import loads, dumps
from os import O_CREAT, O_EXCL, O_WRONLY, open as os_open, fdopen
from base64 import b32encode, b32decode

import attr
from attr import validators

from eliot import start_action

from twisted.python.url import URL
from twisted.web.iweb import IAgent, IResponse
from twisted.web.resource import Resource
from twisted.web.http import CREATED, NO_CONTENT, OK
from twisted.web.server import Site
from twisted.internet import task as theCooperator
from twisted.web.client import FileBodyProducer, readBody
from twisted.python.usage import Options as _Options, UsageError
from twisted.python.filepath import FilePath
from twisted.application.internet import StreamServerEndpointService
from twisted.internet.endpoints import serverFromString

from .model import NullDeploymentConfiguration, SubscriptionDetails
from .server import new_tahoe_configuration, secrets_to_legacy_format

from lae_util import validators as my_validators
from lae_util.fileutil import make_dirs
from lae_util.memoryagent import MemoryAgent
from lae_util.uncooperator import Uncooperator

def create(path):
    flags = (
        # Create the subscription file
        O_CREAT
        # Fail if it already exists
        | O_EXCL
        # Open it for writing only
        | O_WRONLY
    )
    return fdopen(os_open(path.path, flags), "w")


class Subscriptions(Resource):
    """
    GET / -> list of subscription identifiers
    PUT /<subscription id> -> create new subscription
    DELETE /<subscription id> -> cancel an existing subscription
    """
    def __init__(self, database):
        Resource.__init__(self)
        self.database = database

    def getChild(self, name, request):
        return Subscription(self.database, name)

    def render_GET(self, request):
        ids = self.database.list_active_subscription_identifiers()
        subscriptions = list(
            marshal_subscription(self.database.get_subscription(sid))
            for sid
            in ids
        )
        request.responseHeaders.setRawHeaders(u"content-type", [u"application/json"])
        return dumps(dict(subscriptions=subscriptions))


def _marshal_oldsecrets(oldsecrets):
    oldsecrets["introducer_node_pem"] = "".join(map(str, oldsecrets["introducer_node_pem"]))
    oldsecrets["server_node_pem"] = "".join(map(str, oldsecrets["server_node_pem"]))
    return oldsecrets


def marshal_subscription(details):
    result = attr.asdict(details)
    if result["oldsecrets"]:
        result["oldsecrets"] = _marshal_oldsecrets(result["oldsecrets"])
    return result


class Subscription(Resource):
    def __init__(self, database, subscription_id):
        Resource.__init__(self)
        self.database = database
        self.subscription_id = subscription_id

    def render_PUT(self, request):
        payload = loads(request.content.read())
        details = self.database.create_subscription(
            subscription_id=self.subscription_id,
            details=SubscriptionDetails(**payload),
        )
        request.setResponseCode(CREATED)
        return dumps(attr.asdict(details))

    def render_GET(self, request):
        details = self.database.get_subscription(
            subscription_id=self.subscription_id
        )
        request.setResponseCode(OK)
        return dumps(marshal_subscription(details))

    def render_DELETE(self, request):
        self.database.deactivate_subscription(subscription_id=self.subscription_id)
        request.setResponseCode(NO_CONTENT)
        return b""


# XXX Just filesystem based for now (easier to get right quickly;
# dunno what database makes sense yet, etc).  At some point, put a
# real database here.
@attr.s(frozen=True)
class SubscriptionDatabase(object):
    path = attr.ib(validator=my_validators.all(
        validators.instance_of(FilePath),
        my_validators.after(
            lambda i, a, v: v.basename(),
            validators.instance_of(unicode),
        ),
    ))

    @classmethod
    def from_directory(cls, path):
        if not path.exists():
            raise ValueError("State directory ({}) does not exist.".format(path.path))
        if not path.isdir():
            raise ValueError("State path ({}) is not a directory.".format(path.path))
        return SubscriptionDatabase(path=path)

    def _subscription_path(self, subscription_id):
        return self.path.child(b32encode(subscription_id) + u".json")

    def _subscription_state(self, subscription_id, details):
        return dict(
            version=1,
            details=dict(
                active=True,
                id=subscription_id,

                bucket_name=details.bucketname,
                oldsecrets=_marshal_oldsecrets(details.oldsecrets),
                email=details.customer_email,

                product_id=details.product_id,
                customer_id=details.customer_id,
                subscription_id=details.subscription_id,

                introducer_port_number=details.introducer_port_number,
                storage_port_number=details.storage_port_number,
            ),
        )


    def _create(self, path, content):
        with create(path) as subscription_file:
            # XXX Crash here and we have inconsistent state on disk.
            # It would be better to write to a temporary file and then
            # renameat2(..., RENAME_NOREPLACE) but Python doesn't
            # expose that API.
            #
            # At least we can dump the whole config in memory and then
            # write it in one go.
            subscription_file.write(content)


    def _assign_addresses(self):
        with start_action(action_type=u"subscription-database:assign-addresses") as a:
            subscription_count = len(self.path.listdir())

            start_port = 10000
            end_port = 65535

            first_port = start_port + 2 * subscription_count
            if first_port > end_port:
                # We ran out of ports to allocate.
                raise Exception("We ran out of ports to allocate.")

            result = dict(
                introducer_port_number=first_port,
                storage_port_number=first_port + 1,
            )
            a.add_success_fields(**result)
            return result


    def create_subscription(self, subscription_id, details):
        a = start_action(
            action_type=u"subscription-database:create-subscription",
            id=subscription_id,
            details=attr.asdict(details),
        )
        with a:
            path = self._subscription_path(subscription_id)
            details = attr.assoc(details, **self._assign_addresses())
            if details.oldsecrets:
                raise Exception(
                    "You supplied secrets (%r) but that's nonsense!" % (
                        details.oldsecrets,
                    ),
                )
            # XXX new_tahoe_configuration still pulls some secrets off this
            # object.  That's fine for now but it's just another example of
            # how screwed up our secret/config management is.  Someone else
            # will fix up the fact that we're getting bogus values off the
            # NullDeploymentConfiguration here.  We don't really *want* this
            # global configuration persisted alongside each subscription,
            # anyway
            deploy_config = NullDeploymentConfiguration()
            from .subscription_converger import _introducer_name_for_subscription
            config = new_tahoe_configuration(
                deploy_config,
                details.bucketname,
                unicode(_introducer_name_for_subscription(
                    details.subscription_id, u"leastauthority.com.",
                )),
                u"127.0.0.1",
                details.introducer_port_number,
            )
            legacy = secrets_to_legacy_format(config)
            details = attr.assoc(details, oldsecrets=legacy)
            state = self._subscription_state(subscription_id, details)
            self._create(path, dumps(state))
            return details


    def deactivate_subscription(self, subscription_id):
        path = self._subscription_path(subscription_id)
        subscription = loads(path.getContent())
        subscription["details"]["active"] = False
        path.setContent(dumps(subscription))

    def get_subscription(self, subscription_id):
        path = self._subscription_path(subscription_id)
        state = loads(path.getContent())
        return getattr(self, "_load_{}".format(state["version"]))(state)

    def _load_1(self, state):
        details = state["details"]
        return SubscriptionDetails(
            bucketname=details["bucket_name"],
            oldsecrets=details["oldsecrets"],
            customer_email=details["email"],
            customer_pgpinfo=None,

            product_id=details["product_id"],
            customer_id=details["customer_id"],
            subscription_id=details["subscription_id"],

            introducer_port_number=details["introducer_port_number"],
            storage_port_number=details["storage_port_number"],
        )

    def list_all_subscription_identifiers(self):
        return (
            b32decode(child.basename()[:-len(u".json")])
            for child in self.path.children()
        )

    def list_active_subscription_identifiers(self):
        return list(
            b32decode(child.basename()[:-len(u".json")])
            for child in self.path.children()
            if loads(child.getContent())["details"]["active"]
        )


def required(options, key):
    if options[key] is None:
        raise UsageError("--{} is required.".format(key))


def make_resource(path):
    v1 = Resource()
    v1.putChild("subscriptions", Subscriptions(SubscriptionDatabase.from_directory(path)))

    root = Resource()
    root.putChild("v1", v1)

    return root


class Options(_Options):
    optParameters = [
        ("state-path", "p", None, "Path to the subscription state directory."),
        ("listen-address", "l", None, "Endpoint on which the server should listen."),
    ]

    def postOptions(self):
        required(self, "state-path")
        required(self, "listen-address")
        self["state-path"] = FilePath(self["state-path"].decode("utf-8"))

def makeService(options):
    """
    Make a new subscription manager ``IService``.
    """
    from twisted.internet import reactor

    make_dirs(options["state-path"].path)
    site = Site(make_resource(options["state-path"]))

    return StreamServerEndpointService(
        serverFromString(reactor, options["listen-address"]),
        site,
    )


def decode_subscription(fields):
    return SubscriptionDetails(**fields)

@attr.s
class Client(object):
    endpoint = attr.ib(validator=validators.instance_of(bytes))
    agent = attr.ib(validator=validators.provides(IAgent))
    cooperator = attr.ib()

    def _url(self, *segments):
        return URL.fromText(self.endpoint.decode("utf-8")).child(*segments).asURI().asText().encode("ascii")


    def create(self, subscription_id, details):
        """
        Create a new, active subscription.

        :param unicode subscription_id: The unique identifier for this
        new subscription.

        :param SubscriptionDetails details: The details of the
        subscription.

        :return: A ``Deferred`` that fires when the subscription has
        been created.
        """
        d = self.agent.request(
            b"PUT", self._url(u"v1", u"subscriptions", subscription_id),
            bodyProducer=FileBodyProducer(
                BytesIO(dumps(marshal_subscription(details))),
                cooperator=self.cooperator,
            ),
        )
        d.addCallback(require_code(CREATED))
        d.addCallback(readBody)
        d.addCallback(lambda body: SubscriptionDetails(**loads(body)))
        return d

    def get(self, subscription_id):
        """
        Get an existing subscription, either active or inactive.

        :param unicode subscription_id: The unique identifier of the
        subscription to retrieve.

        :return: A ``Deferred`` that fires with a
        ``SubscriptionDetails`` instance describing the identified
        subscription.
        """
        d = self.agent.request(
            b"GET", self._url(u"v1", u"subscriptions", subscription_id),
        )
        d.addCallback(require_code(OK))
        d.addCallback(readBody)
        d.addCallback(loads)
        d.addCallback(decode_subscription)
        return d

    def list(self):
        """
        Get all existing active subscriptions.
        """
        d = self.agent.request(
            b"GET", self._url(u"v1", u"subscriptions"),
        )
        d.addCallback(require_code(OK))
        d.addCallback(readBody)
        d.addCallback(loads)
        d.addCallback(lambda response: response["subscriptions"])
        d.addCallback(lambda json: map(decode_subscription, json))
        return d

    def delete(self, subscription_id):
        d = self.agent.request(
            b"DELETE", self._url(u"v1", u"subscriptions", subscription_id),
        )
        d.addCallback(require_code(NO_CONTENT))
        d.addCallback(lambda ignored: None)
        return d


@attr.s
class UnexpectedResponseCode(Exception):
    response = attr.ib(validator=validators.provides(IResponse))
    required = attr.ib(validator=validators.instance_of(int))

def require_code(required):
    def check(response):
        if response.code != required:
            raise UnexpectedResponseCode(response, required)
        return response
    return check


def network_client(endpoint, agent, cooperator=None):
    """
    Create a subscription manager client which uses the given
    ``IAgent`` provider to interact with a subscription manager
    server.
    """
    if cooperator is None:
        cooperator = theCooperator
    return Client(endpoint=endpoint, agent=agent, cooperator=cooperator)


def memory_client(database_path):
    """
    Create a subscription manager client which uses in-memory
    interactions with the database at the given path.
    """
    root = make_resource(database_path)
    agent = MemoryAgent(root)
    return Client(endpoint=b"/", agent=agent, cooperator=Uncooperator())


def broken_client():
    agent = MemoryAgent(Resource())
    return Client(endpoint=b"", agent=agent, cooperator=Uncooperator())
