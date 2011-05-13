from twisted.trial.unittest import TestCase

import mock

from lae_site.util.timestamp import ISO_TIME_FMT, now, format_iso_time



class TimeTests (TestCase):

    @mock.patch('time.strftime')
    def test_time(self, strftime):

        now()
        strftime.assert_called_with(ISO_TIME_FMT)

        t = mock.sentinel.FAKE_TIME
        format_iso_time(t)
        strftime.assert_called_with(ISO_TIME_FMT, t)

