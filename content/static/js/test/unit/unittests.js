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
    var asideStripe, aside$;
    beforeEach(function () {
	aside$ = window.$;
	window.$ = {};
	asideStripe = window.Stripe;
	window.Stripe = {};
    });

    afterEach( function () {
	window.Stripe = asideStripe;
	asideStripe = {};
	window.$ = aside$;
	aside$ = {};
    });

    it('registers the response handler', function () {
	window.Stripe = { createToken : function($form, responsehandler){} };
	var HTMLformSpy = jasmine.createSpy("HTMLform");
	var button = jasmine.createSpyObj('button', ['prop'])
	var jqueryform = jasmine.createSpyObj('form', ['find']);
	jqueryform.find.andReturn(button)
	window.$ = jasmine.createSpy("$").andReturn(jqueryform);
	spyOn(window.Stripe, 'createToken');
	creditcardVerifier.formSubmissionHandler.call( HTMLformSpy, jasmine.createSpy("event") );
	expect(Stripe.createToken).toHaveBeenCalledWith( jasmine.any(Object), creditcardVerifier.stripeResponseHandler );
	expect(button.prop).toHaveBeenCalledWith('disabled', true);
    }); 
});