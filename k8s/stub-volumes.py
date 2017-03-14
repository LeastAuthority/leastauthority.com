#!/usr/bin/env python

from sys import argv, stdin, stdout
from subprocess import check_output

from yaml import safe_load_all, safe_dump_all

from pyrsistent import ny, freeze, thaw

def main(rev=None):
    if rev is None:
        rev = check_output(["git", "rev-parse", "HEAD"]).strip()[:7]
    stdout.write(safe_dump_all(stub_all_volumes(rev, list(safe_load_all(stdin)))))


def stub_all_volumes(rev, docs):
    def deployments(index):
        return docs[index].get(u"kind") == u"Deployment"

    def persistent_volume_claim(volume):
        return u"persistentVolumeClaim" in volume

    def to_empty_dir(volume):
        return {u"name": volume[u"name"], u"emptyDir": {}}

    def specified_tag(image):
        return u":".join((image.rsplit(u":", 1)[0], rev))

    def if_(predicate, thunk):
        def xform(value):
            if predicate(value):
                return thunk(value)
            return value
        return xform

    return thaw(
        freeze(docs).transform(
            [deployments, u"spec", u"template", u"spec", u"volumes", ny],
            if_(persistent_volume_claim, to_empty_dir),
            [deployments, u"spec", u"template", u"spec", u"containers", ny, u"image"],
            specified_tag,
        )
    )

main(*argv[1:])
