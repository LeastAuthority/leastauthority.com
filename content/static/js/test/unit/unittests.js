"use strict";

describe('callback registration', function () {
    it('should be defined', function () {
	expect(creditcardVerifier).toBeDefined();
	expect(creditcardVerifier.formSubmissionHandler).toBeDefined();
	expect(creditcardVerifier.stripeResponseHandler).toBeDefined();
	expect(creditcardVerifier.register_callbacks).toBeDefined();
    });
});

describe('The form submission handler', function () { 
    var asideStripe, aside$, jqueryform, HTMLformSpy, button;
    beforeEach(function () {
	aside$ = window.$;
	window.$ = {};
	asideStripe = window.Stripe;
	window.Stripe = {};
	window.Stripe = jasmine.createSpyObj('Stripe', ['createToken']);
	jqueryform = jasmine.createSpyObj('form', ['find']);
	window.$ = jasmine.createSpy("$").andReturn(jqueryform);
	HTMLformSpy = jasmine.createSpy("HTMLform");
	button = jasmine.createSpyObj('button', ['prop'])
	jqueryform.find.andReturn(button)

	creditcardVerifier.formSubmissionHandler.call( HTMLformSpy, jasmine.createSpy("click") );
    });

    afterEach( function () {
	window.Stripe = asideStripe;
	window.$ = aside$;
	asideStripe, aside$, jqueryform, HTMLformSpy, button = {};
    });

    it('finds the form submission button', function () {
	expect(jqueryform.find).toHaveBeenCalledWith('button');
    }); 

    it('disables the form submission button', function () {
	expect(button.prop).toHaveBeenCalledWith('disabled', true);
    }); 

    it('registers the response handler inside Stripe.createToken', function () {
	expect(Stripe.createToken).toHaveBeenCalledWith( jqueryform, creditcardVerifier.stripeResponseHandler );
    }); 
});