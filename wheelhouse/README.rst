What's This
-----------

This directory contains packages we want pypi2nix to use during its requirements.nix-generation stages.
These packages won't actually end up in the resulting Nix derivation, though.

Packages
--------

unittest2
~~~~~~~~~
This package has a patched setup.py to hard-code a particular version number.
The upstream 1.1.0 sdist release cannot have a wheel built from it due to semi-recent setuptools changes.
https://github.com/testing-cabal/unittest-ext/issues/101
