
from mock import Mock
from twisted.trial.unittest import TestCase

from lae_util.servers import append_record


TESTTUPLE = ('One', 'Two')

class ServersTests (TestCase):
    def test_append_record(self):
        mockfp = Mock()
        mockopen = mockfp.open
        mockhandle = mockfp.open.return_value
        mockwrite = mockhandle.write
        mockclose = mockhandle.close

        append_record(mockfp, *TESTTUPLE)
        mockopen.assert_called_with('a')
        writevector = mockwrite.call_args[0][0]
        self.failUnlessEqual(len(writevector.split(',')), 1+len(TESTTUPLE))
        mockclose.assert_called_with()
