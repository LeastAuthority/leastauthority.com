# create-plans

Create Chargebee Product Plans for S4.

    $ stack build
    $ stack exec create-plans-exe

Plans to create are defined by `s4Plans` in `src/Lib.hs`.

# Payment Gateway

ChargeBee does not offer an API for configuring payment gateways.
Therefore, a payment gateway must be configured manually.
To configure a Stripe payment gateway:

1. Log in to Stripe as an "Admin" user.
2. Make sure that the desired Stripe account is "active"
   (select it from the account drop-down).
   ChargeBee will attempt to use whatever account is "active" for the next steps.
3. Log in to ChargeBee as a "Full Access" user.
4. Visit `Settings -> Billing -> Payment gateways`
5. Select Stripe.
6. Select "Connect to an Existing Account"
7. Fill out a Stripe account activation form if necessary.
8. Configure the gateway in ChargeBee.
   Assign a descriptive "Display Name" to the gateway
   (otherwise it will be difficult to determine where the gateway leads later).
   Click the "Update" button when all configuration is complete.
9. Configure webhook settings in Stripe.
   Follow the instructions below the "Update" button.
