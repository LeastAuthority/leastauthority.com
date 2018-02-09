# Copyright Least Authority Enterprises.
# See LICENSE for details.

# Try to avoid creating a zillion zillion garbage directories right in
# /tmp.  Instead, make one per run and get the rest of the test suite
# to use it.
import tempfile
tempfile.tempdir = tempfile.mkdtemp()
del tempfile

from hypothesis import settings
settings.register_profile("time-is-an-illusion", settings(deadline=None))
settings.load_profile("time-is-an-illusion")
