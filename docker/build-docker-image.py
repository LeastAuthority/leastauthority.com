from __future__ import print_function, unicode_literals

from io import StringIO
from json import load, dumps, dump
from tarfile import TarInfo, TarFile
from os import environ
from os.path import join
from tempfile import NamedTemporaryFile


def main():
    from pprint import pprint
    pprint(dict(environ))

    out = environ["out"]
    contents = environ["contents"].split()

    config_id = "abcdef"
    layers = list(
        load(open(join(content, "json")))
        for content
        in contents
    )

    with TarFile(out, "w") as image:
        with NamedTemporaryFile() as config_file:
            dump(image_config(layers), config_file)
            config_file.seek(0)
            info = image.gettarinfo(
                name=config_file.name,
                arcname=config_id + ".json",
            )
            image.addfile(info, config_file)

        with NamedTemporaryFile() as manifest_file:
            dump(image_manifest(layers, {}), manifest_file)
            manifest_file.seek(0)
            info = image.gettarinfo(
                name=manifest_file.name,
                arcname="manifest.json",
            )
            image.addfile(info, manifest_file)

        for layer, derivation in zip(layers, contents):
            image.addfile(
                image.gettarinfo(arcname=layer["id"], name=derivation),
            )
            for entry in ("layer.tar", "json"):
                with open(join(derivation, entry)) as content:
                    info = image.gettarinfo(
                        name=content.name,
                        arcname=join(layer["id"], entry),
                    )
                    image.addfile(info, content)


def image_manifest(layers, config):
    return [{
        "Layers": list(
            layer["id"] + "\\/layer.tar"
            for layer
            in layers
        ),
        "RepoTags": None,
        "Config": config,
    }]



def image_config(layers):
    return {
        "rootfs": {
            # https://gist.github.com/aaronlehmann/b42a2eaf633fc949f93b#id-definitions-and-calculations
            "diff_ids": list(
                "sha256:{}".format(layer["id"])
                for layer
                in layers
            ),
            "type": "layers"
        },
        "os": "linux",
        "history": [],
        "docker_version": "0.0.0",
        "created": "1970-01-01T00:00:00-00:00",
        "container_config": {
            "Labels": None,
            "OnBuild": None,
            "Entrypoint": None,
            "WorkingDir": "",
            "Volumes": None,
            "Image": "sha256:ca817e5c32eaff74ee9dfa6b77c0d030fd8687ca8f6abfba2077268f45e453f8",
            "ArgsEscaped": True,
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
        "container": "",
        "config": {
            "Labels": None,
            "OnBuild": None,
            "Entrypoint": None,
            "WorkingDir": "",
            "Volumes": None,
            "Image": "sha256:ca817e5c32eaff74ee9dfa6b77c0d030fd8687ca8f6abfba2077268f45e453f8",
            "ArgsEscaped": True,
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
        "architecture": "amd64"
    }



main()
