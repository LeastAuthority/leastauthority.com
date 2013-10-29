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
    beforeEach(function () {
	window.Stripe = { createToken : function($form, responsehandler){} };
	spyOn(window.Stripe, 'createToken');
	creditcardVerifier.formSubmissionHandler.call( jasmine.createSpy("this_form"), jasmine.createSpy("event") );
    });

    it('registers the response handler', function () {
	expect(Stripe.createToken).toHaveBeenCalledWith(jasmine.any(Object), creditcardVerifier.stripeResponseHandler);
    });
    
});

/*
describe('The submission handler2', function () { 
    beforeEach(function() {
	spyOn(Stripe, 'createToken');
	window.mockform = jasmine.createSpy("this_form");
	creditcardVerifier.formSubmissionHandler.call(window.mockform, jasmine.createSpy("event"));
    });
    
    it('registers the response handler', function () {
	expect(Stripe.createToken).toHaveBeenCalledWith(jasmine.any(Object), creditcardVerifier.stripeResponseHandler);
    });
});


describe('The submission handler3', function () { 
    window.Stripe = {};
    var Stripe;
    beforeEach(function() {
	spyOn(Stripe, 'createToken');
	mockform = jasmine.createSpy("this_form");
	creditcardVerifier.formSubmissionHandler.call(mockform, jasmine.createSpy("event"));
    });
    
    it('registers the response handler', function () {
	expect(Stripe.createToken).toHaveBeenCalledWith(jasmine.any(Object), creditcardVerifier.stripeResponseHandler);
    });
});*/