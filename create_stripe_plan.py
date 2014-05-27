import stripe

from twisted.python.filepath import FilePath

stripe.api_key = FilePath.getContent('../secret_config/stripeapikey').strip()

stripe.Plan.create(amount=2500,
                   interval=u'month',
                   name=u'LeastAuthority Simple Secure Storage Service (S4)',
                   currency=u'USD',
                   id=u'S4_consumer_iteration_2_beta1_2014-05-27',
                   trial_period_days=30,
                   statement_description=u'S4',
                   )
