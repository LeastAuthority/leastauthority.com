from lae_util import stripe

from twisted.python.filepath import FilePath

stripe.api_key = FilePath('../../k8s_secrets/stripe-private.key').getContent().strip()


amount = 2500
interval = "month"
currency = "USD"
name = "LeastAuthority Secure Simple Storage Service (S4)"
plan_id = "S4_consumer_iteration_2_beta1_2014-05-27"
trial_period_days = 30
statement_descriptor = "S4"

stripe.Plan.create(amount=amount,
                   interval=interval,
                   name=name,
                   currency=currency,
                   id=plan_id,
                   trial_period_days=trial_period_days,
                   statement_descriptor=statement_descriptor)
