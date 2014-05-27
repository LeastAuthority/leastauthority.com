import stripe

stripe.api_key = "sk_test_yQ2hhexxAC6ZSLDp3rnoQTYI"

stripe.Plan.create(amount=2500,
                   interval='month',
                   name='S4',
                   currency='usd',
                   id='S4_01')
