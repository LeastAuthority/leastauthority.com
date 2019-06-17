# Copyright Least Authority Enterprises.
# See LICENSE for details.

__all__ = [
    "with_retry", "retry_if",
    "loop_until", "poll_until",
    "retry_failure", "backoff",
    "timeout",
    "get_default_retry_steps",
    "decorate_methods",

    "opt_metrics_port",
    "AsynchronousService",
]

import stripe
# Pin the Stripe API version to a known value.  This will need to be
# updated from time to time (alongside any code changes that may be
# necessary to retain compatibility with a new Stripe API version).
#
# This overrides the Stripe account-wide API version setting.
stripe.api_version = '2016-07-06'

from ._retry import (
    with_retry, retry_if,
    loop_until, poll_until,
    retry_failure, backoff,
    timeout,
    get_default_retry_steps,
    decorate_methods,
)

from ._prometheus import (
    opt_metrics_port,
)

from .service import AsynchronousService

def patch():
    pass

patch()
del patch
