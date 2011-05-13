from twisted.trial.unittest import TestCase

import mock

from lae_site import util



class UtilTests (TestCase):

    @mock.patch('time.strftime')
    def test_time(self, strftime):

        util.now()
        strftime.assert_called_with(util.ISO_TIME_FMT)

        t = mock.sentinel.FAKE_TIME
        util.format_iso_time(t)
        strftime.assert_called_with(util.ISO_TIME_FMT, t)

