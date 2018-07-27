# create-plans

Create Chargebee Product Plans for S4.

    $ stack build
    $ stack exec create-plans-exe

Plans to create are defined by `s4Plans` in `src/Lib.hs`.

# Currencies

ChargeBee does not offer an API for configuring currencies.
Therefore, currencies must be configured manually.
To configure currencies:

1. Log in to ChargeBee as a "Full Access" user.
2. Visit `Settings -> Billing -> Currencies`
3. Select the desired "Base Currency".
4. If prompted to enable multicurrency, do so.
5. Add as many additional currencies as desired.

`create-plan-exe` will fail if it is run before all of the currencies it references are enabled.

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
