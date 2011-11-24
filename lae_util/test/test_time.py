from twisted.trial.unittest import TestCase

from lae_util import timestamp


class TimeTests (TestCase):
    SOME_TIME = 1319843099

    def test_format_iso_time(self):
        t = timestamp.format_iso_time(self.SOME_TIME)
        self.failUnlessEqual(t, "2011-10-28T23:04:59Z")
