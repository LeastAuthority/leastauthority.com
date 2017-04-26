
from __future__ import print_function, unicode_literals

from os import mkdir
from json import dump
from tarfile import TarFile
from hashlib import sha256

from os import environ
from os.path import join


def main():
    from pprint import pprint
    pprint(dict(environ))

    out = environ["out"]
    contents = environ["contents"]

    mkdir(out)

    with TarFile(join(out, "layer.tar"), "w") as layer:
        for content in contents.split():
            layer.add(content)

    layer_hash = sha256()
    with open(join(out, "layer.tar"), "r") as layer:
        while True:
            data = layer.read(2 ** 16)
            if data:
                layer_hash.update(data)
            else:
                break
    layer_id = layer_hash.hexdigest()

    with open(join(out, "VERSION"), "w") as version:
        version.write("1.0")

    with open(join(out, "json"), "w") as config:
        dump(layer_config(layer_id), config)


def layer_config(layer_id):
    return {
        "container_config": {
            "Labels": None,
            "OnBuild": None,
            "Entrypoint": None,
            "WorkingDir": "",
            "Volumes": None,
            "Image": "",
            "Cmd": None,
            "Env": None,
            "StdinOnce": False,
            "OpenStdin": False,
            "Tty": False,
            "AttachStderr": False,
            "AttachStdout": False,
            "AttachStdin": False,
            "User": "",
            "Domainname": "",
            "Hostname": ""
        },
        "created": "1970-01-01T00:00:00-00:00",
        "id": layer_id,
    }


main()
