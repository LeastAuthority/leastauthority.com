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
    var asideStripe;
    beforeEach(function () {
	asideStripe = window.Stripe;
	window.Stripe = {};
    });

    afterEach( function () {
	window.Stripe = asideStripe;
	asideStripe = {};
    });

    it('registers the response handler', function () {
	window.Stripe = { createToken : function($form, responsehandler){} };
	spyOn(window.Stripe, 'createToken');
	creditcardVerifier.formSubmissionHandler.call( jasmine.createSpy("form"), jasmine.createSpy("event") );
	expect(Stripe.createToken).toHaveBeenCalledWith( jasmine.any(Object), creditcardVerifier.stripeResponseHandler );
    }); 
});