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
        format = '%(asctime)s %(levelname) 7s [%(module) 8s L%(lineno)d] %(message)s',
        datefmt = '%Y-%m-%dT%H:%M:%S%z',
        )

    if config.unknown_options:
        logging.warn('Unknown options: %r', config.unknown_options.keys())

    startServer(config)

    reactor.run()


if __name__ == '__main__':
    main()
