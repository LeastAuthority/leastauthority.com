#!/usr/bin/env python
# Copyright Least Authority Enterprises.
# See LICENSE for details.

from sys import argv, stdin, stdout
from subprocess import check_output

from yaml import safe_load_all, safe_dump_all

from pyrsistent import ny, freeze, thaw

from twisted.python.usage import UsageError, Options

class RewriteOptions(Options):
    optParameters = [
        ("tag", None, None,
         "Rewrite leastauthority.com image tags to the given value "
         "(interpreted by git rev-parse).",
        ),
    ]

    optFlags = [
        ("volumes", None, "Replace PersistentVolumeClaims with emptyDirs."),
    ]



def main(argv):
    o = RewriteOptions()
    try:
        o.parseOptions(argv[1:])
    except UsageError as e:
        raise SystemExit(e)

    docs = freeze(list(safe_load_all(stdin)))

    if o["tag"] is not None:
        tag = check_output(["git", "rev-parse", "--short", o["tag"]]).strip()
        docs = rewrite_tags(docs, tag)

    if o["volumes"]:
        docs = stub_all_volumes(docs)

    stdout.write(safe_dump_all(thaw(docs)))



def if_(predicate, thunk):
    def xform(value):
        if predicate(value):
            return thunk(value)
        return value
    return xform



def owned_by(whom):
    def check_owned_by(image):
        parts = image.split(u"/")
        if len(parts) == 2:
            owner = parts[0]
        elif len(parts) == 3:
            owner = parts[1]
        else:
            raise ValueError("Can't parse {!r}".format(image))
        return owner == whom
    return check_owned_by



def deployments(docs):
    def check(index):
        return docs[index].get(u"kind") == u"Deployment"
    return check



def rewrite_tags(docs, rev):
    def specified_tag(image):
        return u":".join((image.rsplit(u":", 1)[0], rev))

    def maybe_change_image(env):
        if env[u"name"].endswith(u"_IMAGE"):
            image = env[u"value"]
            if owned_by(u"leastauthority")(image):
                return env.set(u"value", specified_tag(image))
        return env

    return docs.transform(
        [deployments(docs), u"spec", u"template", u"spec", u"containers", ny, u"image"],
        if_(owned_by("leastauthority"), specified_tag),

        # There are also a couple environment variables that have an image
        # name in them.
        [deployments(docs), u"spec", u"template", u"spec", u"containers", ny, u"env", ny],
        maybe_change_image,
    )



def stub_all_volumes(docs):
    def persistent_volume_claim(volume):
        return u"persistentVolumeClaim" in volume

    def to_empty_dir(volume):
        return {u"name": volume[u"name"], u"emptyDir": {}}

    return docs.transform(
        [deployments(docs), u"spec", u"template", u"spec", u"volumes", ny],
        if_(persistent_volume_claim, to_empty_dir),
    )

main(argv)
