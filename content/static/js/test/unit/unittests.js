"use strict";

describe('callback registration', function () {
    it('should be defined', function () {
	expect(creditcardVerifier).toBeDefined();
	expect(creditcardVerifier.stripeResponseHandler).toBeDefined();
	expect(creditcardVerifier.formSubmissionHandler).toBeDefined();
	expect(creditcardVerifier.register_callbacks).toBeDefined();
    });
});

describe('The form submission handler', function () { 
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

    it('finds the form submission button', function () {
	window.Stripe = jasmine.createSpyObj('Stripe', ['createToken']);
	var jqueryform = jasmine.createSpyObj('form', ['find']);
	window.$ = jasmine.createSpy("$").andReturn(jqueryform);
	var HTMLformSpy = jasmine.createSpy("HTMLform");
	var button = jasmine.createSpyObj('button', ['prop'])
	jqueryform.find.andReturn(button)

	creditcardVerifier.formSubmissionHandler.call( HTMLformSpy, jasmine.createSpy("click") );
	expect(jqueryform.find).toHaveBeenCalledWith('button');
    }); 

    it('disables the form submission button', function () {
	window.Stripe = jasmine.createSpyObj('Stripe', ['createToken']);
	var jqueryform = jasmine.createSpyObj('form', ['find']);
	window.$ = jasmine.createSpy("$").andReturn(jqueryform);
	var HTMLformSpy = jasmine.createSpy("HTMLform");
	var button = jasmine.createSpyObj('button', ['prop'])
	jqueryform.find.andReturn(button)

	creditcardVerifier.formSubmissionHandler.call( HTMLformSpy, jasmine.createSpy("click") );
	expect(button.prop).toHaveBeenCalledWith('disabled', true);
    }); 

    it('registers the response handler inside Stripe.createToken', function () {
	window.Stripe = jasmine.createSpyObj('Stripe', ['createToken']);
	var jqueryform = jasmine.createSpyObj('form', ['find']);
	window.$ = jasmine.createSpy("$").andReturn(jqueryform);
	var HTMLformSpy = jasmine.createSpy("HTMLform");
	var button = jasmine.createSpyObj('button', ['prop'])
	jqueryform.find.andReturn(button)

	creditcardVerifier.formSubmissionHandler.call( HTMLformSpy, jasmine.createSpy("click") );
	expect(Stripe.createToken).toHaveBeenCalledWith( jqueryform, creditcardVerifier.stripeResponseHandler );
    }); 


});