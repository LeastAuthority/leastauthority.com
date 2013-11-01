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
	/*var buttonSpy = jasmine.createSpyObj("button", ["prop"]);
	, ["find"]).andReturn(buttonSpy);
	var jquerySpy = jasmine.createSpy("$").andReturn(formSpy);
	*/
	var HTMLformSpy = jasmine.createSpy("HTMLform");
	var formbutton = {prop : function(boolstatus, setboolstatus){}}
	var jqueryform = {find : function(target) 
			  {
			      if (target === "button") 
			      {
				  return formbutton;
			      }
			  }
			 }
	window.$ = jasmine.createSpy("$").andReturn(jqueryform);
	//alert($);
	spyOn(window.Stripe, 'createToken');
	creditcardVerifier.formSubmissionHandler.call( HTMLformSpy, jasmine.createSpy("event") );
	expect(Stripe.createToken).toHaveBeenCalledWith( jasmine.any(Object), creditcardVerifier.stripeResponseHandler );
    }); 
});