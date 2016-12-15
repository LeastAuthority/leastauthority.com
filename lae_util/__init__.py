# Copyright Least Authority Enterprises.
# See LICENSE for details.

__all__ = [
    "with_retry", "retry_if",
    "loop_until", "poll_until",
    "retry_failure", "backoff",
    "timeout",
    "get_default_retry_steps",
    "decorate_methods",
]

import stripe
# Pin the Stripe API version to a known value.  This will need to be
# updated from time to time (alongside any code changes that may be
# necessary to retain compatibility with a new Stripe API version).
#
# This overrides the Stripe account-wide API version setting.
stripe.api_version = '2016-07-06'

def _redirect_eliot_logs_for_trial():
    """
    Enable Eliot logging to the ``_trial/test.log`` file.

    This wrapper function allows lae_util/__version__.py to be
    imported by packaging tools without them having to install Eliot
    and its dependencies.
    """
    import os
    import sys
    if os.path.basename(sys.argv[0]) == "trial":
        from eliot.twisted import redirectLogsForTrial
        redirectLogsForTrial()
_redirect_eliot_logs_for_trial()
del _redirect_eliot_logs_for_trial

from ._retry import (
    with_retry, retry_if,
    loop_until, poll_until,
    retry_failure, backoff,
    timeout,
    get_default_retry_steps,
    decorate_methods,
)
