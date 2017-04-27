from __future__ import print_function, unicode_literals

from json import load, dump
from hashlib import sha256
from tarfile import TarFile
from os import mkdir, environ
from os.path import join
from tempfile import NamedTemporaryFile


def main():
    out = environ["out"]
    referencePath = environ["referencePath"]

    with open(out, "wb") as image_file:
        build_docker_image(referencePath, image_file)


def build_docker_image(referencePath, image_file):
    with open(referencePath) as references_file:
        references = parse_references(references_file)

    config_id = "abcdef"
    layers = build_docker_layers(references)
    config_path = config_id + ".json"

    with TarFile(fileobj=image_file, mode="w") as image:
        with NamedTemporaryFile() as config_file:
            dump(image_config(layers), config_file)
            config_file.seek(0)
            info = image.gettarinfo(
                name=config_file.name,
                arcname=config_path,
            )
            image.addfile(info, config_file)

        with NamedTemporaryFile() as manifest_file:
            dump(image_manifest(layers, config_path), manifest_file)
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
            layer["id"] + "/layer.tar"
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
            "Env": [],
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
            "Cmd": [],
            "Env": [],
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



def parse_references(fobj):
    references = set()
    for line in (line.strip() for line in fobj):
        if line.isdigit() or line.isspace():
            continue
        references.add(line)
    return references



def build_docker_layers(references):
    return list(
        build_docker_layer(derivations)
        for derivations
        in group_references(references, max_groups=120)
    )



def group_references(references, max_groups):
    assert len(references) < max_groups
    return list(
        [ref]
        for ref
        in references
    )



def build_docker_layer(derivations, out):
    mkdir(out)

    archive_path = join(out, "layer.tar")
    with TarFile(archive_path, "w") as layer:
        for content in derivations:
            layer.add(content)

    with open(archive_path, "r") as layer:
        layer_id = get_layer_id(layer)

    with open(join(out, "VERSION"), "w") as version:
        version.write("1.0")

    with open(join(out, "json"), "w") as config:
        dump(layer_config(layer_id), config)


def get_layer_id(layer):
    # https://gist.github.com/aaronlehmann/b42a2eaf633fc949f93b#id-definitions-and-calculations
    layer_hash = sha256()
    while True:
        data = layer.read(2 ** 16)
        if data:
            layer_hash.update(data)
        else:
            break
    return layer_hash.hexdigest()



main()
