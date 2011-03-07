from twisted.web.server import Site
from twisted.web.static import File
from twisted.internet import reactor

resource = File('content')
factory = Site(resource)
reactor.listenTCP(80, factory)
reactor.run()
