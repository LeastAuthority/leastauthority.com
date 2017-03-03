from setuptools import find_packages, setup

setup(
    name="leastauthority.com",
    packages=find_packages(),
    package_data={
        # Templates for the website.
        "lae_site": ["*.html"],
        # Helper for starting tahoe-lafs with configuration from our system.
        "lae_automation": ["configure-tahoe"],
    },
    dependency_links=[
        "https://tahoe-lafs.org/deps/",
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
        "twisted",
        "service_identity",
        "attrs",
        "eliot==0.12.0",

        "txAWS==0.2.1.post5",

        # For the test suite.
        "hypothesis==3.6.0",
        "testtools==2.2.0",
        "fixtures==3.0.0",

        # So we can generate tahoe configuration parameters.
        "tahoe-lafs==1.11.0",
    ],
)
