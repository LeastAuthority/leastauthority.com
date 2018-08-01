from twisted.python.reflect import namedAny

def detect():
    # On some versions of Twisted, must import twisted.trial.unittest first or
    # this import always fails.
    namedAny("twisted.trial.unittest")
    try:
        from twisted.trial._dist.workerreporter import WorkerReporter
    except ImportError:
        return False

    from unittest import TestCase
    from twisted.protocols.amp import AMP
    from twisted.python.failure import Failure

    case = TestCase("run")
    reporter = WorkerReporter(AMP())
    failure = Failure(Exception(u"\N{SNOWMAN}".encode("utf-8")))
    try:
        reporter.addFailure(case, failure)
    except UnicodeDecodeError:
        return True
    return False


def patch():
    def addFailure(self, test, fail):
        """
        Send a Failure over.
        """
        super(WorkerReporter, self).addFailure(test, fail)
        testName = test.id()
        if isinstance(testName, unicode):
            testName = testName.encode("utf-8")
        failure = self._getFailure(fail)
        fail = failure.getErrorMessage()
        if isinstance(fail, unicode):
            fail = fail.encode("utf-8")
        failClass = qual(failure.type).encode("utf-8")
        frames = [frame.encode("utf-8") for frame in self._getFrames(failure)]
        self.ampProtocol.callRemote(managercommands.AddFailure,
                                    testName=testName,
                                    fail=fail,
                                    failClass=failClass,
                                    frames=frames)


    from twisted.python.compat import unicode
    from twisted.python.reflect import qual
    from twisted.trial._dist.workerreporter import WorkerReporter
    from twisted.trial._dist import managercommands
    WorkerReporter.addFailure = addFailure
