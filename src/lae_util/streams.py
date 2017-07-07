
class LoggingStream(object):
    """
    I perform unbuffered output to a log with a given prefix on each write.
    """
    def __init__(self, log, prefix):
        self.log = log
        self.prefix = prefix

    def write(self, s):
        if self.prefix:
            self.log.write(self.prefix)
        self.log.write(s)
        if '\n' in s:
            self.log.flush()

    def writelines(self, seq):
        for s in seq:
            self.write(s)

    def flush(self):
        self.log.flush()

    def isatty(self):
        return False

    def close(self):
        self.flush()
