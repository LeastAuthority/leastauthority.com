#!/usr/bin/env python

if __name__ == '__main__':
    from sys import argv
    from twisted.internet.task import react
    from full_signup import main
    react(main, argv[1:])

import sys

from twisted.python.log import startLogging
from twisted.python.usage import UsageError, Options
from twisted.python.filepath import FilePath
from twisted.internet import defer

from lae_util.fileutil import make_dirs
from lae_automation.signup import activate


class SignupOptions(Options):
    optParameters = [
        ("log-directory", None, None, "Path to a directory to which to write signup logs.", FilePath),
        ("secrets-directory", None, None, "Path to a directory to which to write subscription secrets.", FilePath),

        ("automation-config-path", None, None, "Path to an lae_automation_config.json file.", FilePath),
        ("server-info-path", None, None, "Path to a file to write new server details to.", FilePath),

        ("domain", None, None,
         "The domain on which the service is running "
         "(useful for alternate staging deployments).",
        ),
    ]


def main(reactor, *argv):
    o = SignupOptions()
    try:
        o.parseOptions(argv)
    except UsageError as e:
        raise SystemExit(str(e))

    defer.setDebugging(True)

    log_dir = o["log-directory"]
    secrets_dir = o["secrets-directory"]

    for d in [log_dir, secrets_dir]:
        if not log_dir.isdir():
            make_dirs(log_dir.path)

    flapp_stdout_path = log_dir.child('stdout')
    flapp_stderr_path = log_dir.child('stderr')

    startLogging(flapp_stdout_path.open("a"), setStdout=False)

    return activate(
        o["domain"].decode("ascii"), secrets_dir,
        o["automation-config-path"], o["server-info-path"],
        sys.stdin, flapp_stdout_path, flapp_stderr_path,
    )
