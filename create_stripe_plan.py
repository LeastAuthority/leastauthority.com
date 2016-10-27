from lae_util import stripe

from twisted.python.filepath import FilePath

from lae_automation.config import Config

config = Config('../../secret_config/lae_automation_config.json')

stripe.api_key = FilePath('../../secret_config/stripeapikey').getContent().strip()
product = config.products[0]

amount = int(product["amount"])
interval = product["interval"]
currency = product["currency"]
name = product["plan_name"]
plan_id = product["plan_ID"]
trial_period_days = int(product["plan_trial_period_days"])
statement_descriptor = product["statement_description"]

stripe.Plan.create(amount=amount,
                   interval=interval,
                   name=name,
                   currency=currency,
                   id=plan_id,
                   trial_period_days=trial_period_days,
                   statement_descriptor=statement_descriptor)
