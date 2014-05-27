import stripe

from twisted.python.filepath import FilePath

stripe.api_key = FilePath.getContent('../secret_config/stripeapikey').strip()

stripe.Plan.create(amount=2500,
                   interval='month',
                   name='S4',
                   currency='usd',
                   id='S4_01')
