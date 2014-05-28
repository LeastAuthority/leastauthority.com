import stripe

from twisted.python.filepath import FilePath

from lae_automation.config import Config

config = Config()

stripe.api_key = FilePath.getContent('../secret_config/stripeapikey').strip()
amount = config.products[0]["amount"]
interval = config.products[0]["interval"]
currency = config.products[0]["currency"]
name = config.products[0]["plan_name"]
plan_id = config.products[0]["plan_ID"]
trial_period_days = int(config.products[0]["plan_trial_period_days"])

stripe.Plan.create(amount=amount,
                   interval=interval,
                   name=name,
                   currency=currency,
                   id=plan_id,
                   trial_period_days=trial_period_days)
