# Copyright 2014-2016 ClusterHQ
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""
Hypothesis strategies for QuickCheck-style testing.

Originally ``flocker/testtools/strategies.py``.
"""

import string

from hypothesis.strategies import integers, lists, text
from twisted.python.filepath import FilePath


def path_segments(**kw):
    """
    Individual path segments.

    These are UTF-8 encoded segments that contain neither '/' nor NULL.

    e.g. 'foo', 'rc.local'.
    """
    return text(min_size=1, **kw
    ).filter(lambda x: '/' not in x
    ).map(lambda x: x.encode('utf8')
    ).filter(lambda x: '\0' not in x
    )


def paths(**kw):
    """
    Paths

    e.g. ``FilePath('/usr/local')``, ``FilePath('foo/bar/bar')``.
    """
    return lists(path_segments(**kw)).map(lambda ps: FilePath('/'.join(ps)))


_identifier_characters = string.ascii_letters + string.digits + '_'


identifiers = text(
    average_size=20, min_size=1, alphabet=_identifier_characters)
"""
Python identifiers.

e.g. ``Foo``, ``bar``.
"""


fqpns = lists(
    identifiers, min_size=1, average_size=5).map(lambda xs: '.'.join(xs))
"""
Fully-qualified Python names.

e.g. ``twisted.internet.defer.Deferred``, ``foo.bar.baz_qux``.
"""


permissions = integers(min_value=0, max_value=07777)
"""
Unix file permissions.
"""
