from setuptools import find_packages, setup

setup(
    name="leastauthority.com",
    version="2.0",
    zip_safe=False,
    packages=find_packages(),
    py_modules=["twisted.plugins.lae_dropin"],
    include_package_data=True,
    dependency_links=[
        "https://tahoe-lafs.org/deps/",
    ],
    install_requires=[
        "python-dateutil",
        "stripe",
        "pem",
        "foolscap",
        "filepath",
        "jinja2",
        "mock",
        "simplejson",
        "twisted[tls]",
        "attrs",
        "eliot",

        "txAWS",

        "magic-wormhole",

        # If we had a dev extra ourselves, the [dev] part of this would
        # probably be better placed there.
        "txkube[dev]",

        # For the test suite.
        "hypothesis",
        "testtools",
        "fixtures",
        "deepdiff",

        # So we can generate tahoe configuration parameters.
        "tahoe-lafs",
    ],
)
