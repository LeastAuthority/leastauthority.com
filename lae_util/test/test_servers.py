
from mock import patch
from twisted.trial.unittest import TestCase

from lae_util.servers import append_record


TESTTUPLE = ('One', 'Two')

class ServersTests (TestCase):
    @patch('__builtin__.open')
    def test_append_record(self, mockopen):
        mockhandle = mockopen.return_value
        mockwrite = mockhandle.write
        mockclose = mockhandle.close
        append_record('serverinfo.csv', *TESTTUPLE)
        mockopen.assert_called_with('serverinfo.csv', 'a')
        writevector = mockwrite.call_args[0][0]
        self.failUnlessEqual(len(writevector.split(',')), 3)
        mockclose.assert_called_with()
