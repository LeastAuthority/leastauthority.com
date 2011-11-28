
class LoggingTeeStream(object):
    """
    I perform unbuffered output to a stream f, and also to a log with a given
    prefix on each write.
    """
    def __init__(self, f, log, prefix):
        self.f = f
        self.log = log
        self.prefix = prefix

    def write(self, s):
        self.f.write(s)
        self.f.flush()
        self.log.write(self.prefix)
        self.log.write(s)
        self.log.flush()

    def writelines(self, seq):
        for s in seq:
            self.write(s)

    def flush(self):
        self.f.flush()
        self.log.flush()

    def isatty(self):
        return False

    def close(self):
        pass
