"use strict";

window.creditcardVerifier = (function () {
    return {
        register_callbacks: function () {
            var $form = $('#payment-form');
            // This identifies your website in the createToken call below
            Stripe.setPublishableKey('pk_test_IBiTH5UtEo2kB10eb1OSsv0w');
            $form.submit( creditcardVerifier.formSubmissionHandler );
            $form.show();
        },
        formSubmissionHandler: function (event) {
            var $form = $('#payment-form');
            // Disable the submit button to prevent repeated clicks
            $form.find('button').prop('disabled', true);

            Stripe.createToken($form, creditcardVerifier.stripeResponseHandler);
            return false;
        },
        use_pgp: function (checked) {
            var $pgp = $('#pgp');
            if (checked) {
                $pgp.show();
            } else {
                $pgp.hide();
                $('#pgp_pubkey').val('');
            }
            return true;
        },
        stripeResponseHandler: function(status, response) {
            var $form = $('#payment-form');

            if (response.error) {
                // Show the errors on the form
                $form.find('.payment-errors').text(response.error.message);
                $form.find('button').prop('disabled', false);
            } else {
                // token contains id, last4, and card type
                var token = response.id;
                // Insert the token into the form so it gets submitted to the server
                $form.append($('<input type="hidden" name="stripeToken">').val(token));
                // and submit
                $form.get(0).submit();
            }
        }
    }
}());

$(document).ready(creditcardVerifier.register_callbacks);
