"""
*SECURITY*

We use /dev/urandom as an entropy source.  This may be an untrustworthy
source on ec2.  Attackers who happen to control ec2 instances on nearby
hardware may be able to learn information about, or influence, this
entropy source.
"""

from string import hexdigits


class EntropicToken (object):
    """
    Use EntropicToken.generate() to generate a new token.

    Wrap a hex string you believe to be a token with the constructor.

    Retrieve the hex string from an instance with str(token).
    """

    class Malformed (Exception):
        def __init__(self, value):
            Exception.__init__(self, repr(value))

    class InvalidLength (Malformed): pass
    class InvalidContent (Malformed): pass

    BYTE_LENGTH = 16

    _entropyStream = open('/dev/urandom', 'rb')

    @staticmethod
    def generate():
        raw = EntropicToken._entropyStream.read( EntropicToken.BYTE_LENGTH ) 
        return EntropicToken( raw.encode('hex') )


    # Instances:
    __slots__ = ['_hex']

    def __init__(self, hex):
        """
        @param hex: A bytes instance of exactly 2*BYTE_LENGTH length, assumed to be high-entropy hex encoded.

        @raises: EntropicToken.InvalidLength if there are the wrong
                 number of bytes or EntropicToken.InvalidContent if
                 non-hex-digits are present.
        """
        if len(hex) != 2 * EntropicToken.BYTE_LENGTH:
            raise EntropicToken.InvalidLength(hex)

        hex = hex.lower()

        for c in hex:
            if c not in hexdigits:
                raise EntropicToken.InvalidContent(hex)

        self._hex = hex

    def __repr__(self):
        return '<%s %s>' % (type(self).__name__, self._hex)

    def __str__(self):
        return self._hex

    def __eq__(self, other):
        return type(self) == type(other) and self._hex == other._hex
