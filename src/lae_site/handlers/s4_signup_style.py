
from twisted.web.resource import Resource
from twisted.web.util import redirectTo

S4_SIGNUP_STYLE_COOKIE = b"s4_signup_style"

class S4SignupStyle(Resource):
    """
    Plop some state into a browser to control what kind of signup mechanism we
    use when they try to sign up for S4.

    You can point someone at a URL like::

        https://s4.leastauthority.com/s4-signup-style?style=wormhole

    And they will be sent to the signup page and when they signup they will
    follow the magic-wormhole-based path.  This is based on cookies so the
    state will persistent in their browser until they visit another
    ``s4-signup-style`` URL that changes the cookie.
    """
    def render_GET(self, request):
        request.addCookie(
            S4_SIGNUP_STYLE_COOKIE,
            request.args.get(b"style", [b"email"])[0]
        )
        return redirectTo(b"/signup", request)
