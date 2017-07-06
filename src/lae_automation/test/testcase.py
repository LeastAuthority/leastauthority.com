from testtools import TestCase

class TestBase(TestCase):
    def setup_example(self):
        try:
            del self.force_failure
        except AttributeError:
            pass

    def teardown_example(self, ignored):
        if getattr(self, "force_failure", False):
            self.fail("expectation failed")
