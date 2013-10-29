"use strict";

describe('callback registration', function () {
    it('should be defined', function () {
	expect(creditcardVerifier).toBeDefined();
	expect(creditcardVerifier.stripeResponseHandler).toBeDefined();
	expect(creditcardVerifier.formSubmissionHandler).toBeDefined();
	expect(creditcardVerifier.register_callbacks).toBeDefined();
    });
});

describe('The submission handler', function () { 
    var aliasStripe;
    beforeEach(function () {
	aliasStripe = window.Stripe;
	window.Stripe = { createToken : function($form, responsehandler){} };
	spyOn(window.Stripe, 'createToken');
	creditcardVerifier.formSubmissionHandler.call( jasmine.createSpy("this_form"), jasmine.createSpy("event") );
    });

    afterEach( function () {
	window.Stripe = aliasStripe;
	delete window.aliasStripe;
    });

    it('registers the response handler', function () {
	expect(Stripe.createToken).toHaveBeenCalledWith(jasmine.any(Object), creditcardVerifier.stripeResponseHandler);
    }); 
});