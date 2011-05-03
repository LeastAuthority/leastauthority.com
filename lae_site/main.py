#! /usr/bin/env python

import sys
import logging

from twisted.internet import reactor

from lae_site.config import Config
from lae_site.server import startServer


def main():
    config = Config()

    logging.basicConfig(
        stream = sys.stdout,
        level = logging.DEBUG,
        format = '%(asctime)s %(levelname) 5s [%(module)s L%(lineno)d] %(message)s',
        datefmt = '%Y-%m-%dT%H:%M:%S%z',
        )

    startServer(config)

    reactor.run()


if __name__ == '__main__':
    main()
