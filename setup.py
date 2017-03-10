from setuptools import find_packages, setup

setup(
    name="leastauthority.com",
    zip_safe=False,
    packages=find_packages(),
    include_package_data=True,
    dependency_links=[
        "https://tahoe-lafs.org/deps/",
        "https://s3.amazonaws.com/com.leastauthority.wheelhouse/index.html",
        "git+https://github.com/LeastAuthority/txkube.git@master#egg=txkube-17.1.0",
        "git+https://github.com/twisted/txaws.git@master#egg=txAWS-0.3.0",
    ],
    install_requires=[
        "python-dateutil",
        "stripe==1.41.1",
        "pem==16.1.0",
        "foolscap",
        "Fabric",
        "filepath",
        "jinja2",
        "mock",
        "pyOpenSSL",
        "simplejson",
        "twisted!=17.1.0",
        "service_identity",
        "attrs",
        "eliot==0.12.0",

        "txAWS>0.2.3",

        # If we had a dev extra ourselves, the [dev] part of this would
        # probably be better placed there.
        "txkube[dev]",

        # For the test suite.
        "hypothesis==3.6.0",
        "testtools==2.2.0",
        "fixtures==3.0.0",

        # So we can generate tahoe configuration parameters.
        "tahoe-lafs==1.11.0",
    ],
)
