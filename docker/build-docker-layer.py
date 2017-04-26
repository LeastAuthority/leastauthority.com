
from __future__ import print_function, unicode_literals

from os import environ

def main():
    from pprint import pprint
    pprint(dict(environ))

    out = environ["out"]
    contents = environ["contents"]

    for content in contents.split():
        pass

main()
