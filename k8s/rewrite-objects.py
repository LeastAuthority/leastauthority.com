#!/usr/bin/env python
# Copyright Least Authority Enterprises.
# See LICENSE for details.

from sys import argv, stdin, stdout
from subprocess import check_output

from yaml import safe_load_all, safe_dump_all

from pyrsistent import ny, freeze, thaw, discard

from twisted.python.usage import UsageError, Options

class RewriteOptions(Options):
    optParameters = [
        ("tag", None, None,
         "Rewrite leastauthority.com image tags to the given value.",
        ),
        ("git-tag", None, None,
         "Rewrite leastauthority.com image tags to the given value."
         "(interpreted by git rev-parse).",
        ),
    ]

    optFlags = [
        ("no-volumes", None, "Replace PersistentVolumeClaims with emptyDirs."),
    ]



def main(argv):
    o = RewriteOptions()
    try:
        o.parseOptions(argv[1:])
    except UsageError as e:
        raise SystemExit(e)

    docs = freeze(list(safe_load_all(stdin)))

    if o["git-tag"] is not None:
        tag = check_output(["git", "rev-parse", "--short", o["git-tag"]]).strip()
        docs = rewrite_tags(docs, tag)
    elif o["tag"] is not None:
        docs = rewrite_tags(docs, o["tag"])

    if o["no-volumes"]:
        docs = stub_all_volumes(docs)

    stdout.write(safe_dump_all(thaw(docs)))



def if_(predicate, thunk):
    def xform(value):
        if predicate(value):
            return thunk(value)
        return value
    return xform


def and_(*predicates):
    def combined(value):
        return all(predicate(value) for predicate in predicates)
    return combined


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



def k8s_resource_filter(kind):
    def check(index, value):
        return value.kind == kind
    return check


deployments = k8s_resource_filter(u"Deployment")
pvcs = k8s_resource_filter(u"PersistentVolumeClaim")
pvs = k8s_resource_filter(u"PersistentVolume")


def has_tag():
    def has_tag_predicate(value):
        return u":" in value
    return has_tag_predicate


def rewrite_tags(docs, rev):
    def specified_tag(image):
        return u":".join((image.rsplit(u":", 1)[0], rev))

    return docs.transform(
        [deployments, u"spec", u"template", u"spec", u"containers", ny, u"image"],
        if_(
            and_(
                owned_by("leastauthority"),
                # Give us an out from this rewriting.  If no tag is specified
                # at all, leave it alone.  Sad hack.  Need to do better.
                has_tag(),
            ),
            specified_tag,
        ),
    )



def stub_all_volumes(docs):
    def persistent_volume_claim(volume):
        return u"persistentVolumeClaim" in volume

    def to_empty_dir(volume):
        return {u"name": volume[u"name"], u"emptyDir": {}}

    return docs.transform(
        [deployments, u"spec", u"template", u"spec", u"volumes", ny],
        if_(persistent_volume_claim, to_empty_dir),

        [pvcs],
        discard,

        [pvs],
        discard,
    )

main(argv)
