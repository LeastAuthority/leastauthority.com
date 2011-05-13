from StringIO import StringIO

from twisted.trial.unittest import TestCase

from lae_site.entropy import EntropicToken


class EntropicTokenTests (TestCase):

    _FAKE_STREAM_CONTENT = bytes( ''.join( [ chr(i) for i in range(256) ] ) )

    _FIRST_TOKEN_HEX = _FAKE_STREAM_CONTENT [ : EntropicToken.BYTE_LENGTH ].encode('hex')


    def setUp(self):
        # Monkey-patch the entropy source for deterministic tests:
        EntropicToken._entropyStream = StringIO(self._FAKE_STREAM_CONTENT)


    def test_negative_wrapIncorrectSize(self):
        vector = 'I am too short.'

        assert len(vector) < 2 * EntropicToken.BYTE_LENGTH

        try:
            et = EntropicToken( vector )
        except EntropicToken.InvalidLength, e:
            expected = ( repr(vector), )
            self.assertEqual( expected, e.args )
        else:
            self.fail('Invalid length not detected: EntropicToken(%r) -> %r' % (vector, et))


    def test_negative_non_hex_digits(self):
        vector = 'proper length with non hex chars'

        assert len(vector) == 2 * EntropicToken.BYTE_LENGTH

        try:
            et = EntropicToken( vector )
        except EntropicToken.InvalidContent, e:
            expected = ( repr(vector), )
            self.assertEqual( expected, e.args )
        else:
            self.fail('Invalid length not detected: EntropicToken(%r) -> %r' % (vector, et))


    def test_generate(self):

        expected = self._FIRST_TOKEN_HEX

        et = EntropicToken.generate()

        self.assertEqual( expected, str(et) )


    def test_wrapWithConstructor(self):

        expected = self._FIRST_TOKEN_HEX

        et = EntropicToken( self._FIRST_TOKEN_HEX )

        self.assertEqual( expected, str(et) )


    def test_clone(self):

        et1 = EntropicToken.generate()
        et2 = EntropicToken ( str(et1) )

        self.assertEqual(et1, et2)


    def test_repr(self):

        expected = '<%s %s>' % (
            EntropicToken.__name__,
            self._FIRST_TOKEN_HEX,
            )

        et = EntropicToken.generate()

        self.assertEqual ( expected, repr(et) )


