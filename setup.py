from setuptools import find_packages, setup

setup(
    name="leastauthority.com",
    version="2.0",
    zip_safe=False,
    package_dir={"": "src"},
    packages=find_packages(where="src"),
    py_modules=["twisted.plugins.lae_dropin"],
    include_package_data=True,
    entry_points={
        "console_scripts": [
            "cancel-subscription = lae_automation.opstools:cancel_subscription_main",
            "sync-subscriptions-to-stripe = lae_automation.opstools:sync_subscriptions_main",
            "copy-subscriptions-to-account = lae_automation.opstools:copy_subscriptions_to_account",
        ],
    },
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
        "simplejson",
        "twisted[tls]",
        "attrs",
        "eliot",

        "txAWS",
        "prometheus_client",

        "magic-wormhole",

        "txkube",

        # So we can generate tahoe configuration parameters.
        "tahoe-lafs",
    ],
    extras_require={
        "dev": [
            "mock",
            "txkube[dev]",
            "hypothesis",
            "testtools",
            "fixtures",
            "deepdiff",

            # This lets us generate our Grafana dashboard configurations.
            # It's really part of the ops/monitoring system and not S4.
            # Consider splitting the ops stuff off into another repo.
            "grafanalib",
        ],
    },
)
