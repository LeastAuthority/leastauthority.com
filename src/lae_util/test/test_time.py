from twisted.trial.unittest import TestCase

from lae_util import timestamp


class TimeTests (TestCase):
    SOME_TIME = 1319843099
    SOME_TIMESTAMP = "2011-10-28T23:04:59Z"
    SOME_TIMESTAMP2 = "2011-10-28T23:04:59.000Z"

    def test_format_iso_time(self):
        t = timestamp.format_iso_time(self.SOME_TIME)
        self.failUnlessEqual(t, self.SOME_TIMESTAMP)

    def test_parse_iso_time(self):
        secs = timestamp.parse_iso_time(self.SOME_TIMESTAMP)
        self.failUnlessEqual(secs, self.SOME_TIME)

        secs2 = timestamp.parse_iso_time(self.SOME_TIMESTAMP2)
        self.failUnlessEqual(secs2, self.SOME_TIME)

        self.failUnlessRaises(ValueError, timestamp.parse_iso_time, "2012-00-00T00:00:00Z")
        self.failUnlessRaises(ValueError, timestamp.parse_iso_time, "not a timestamp")
