"use strict";

describe('The register_callbacks callback', function () {
    var asideStripe, aside$, jqueryform;
    beforeEach(function () {
	aside$ = window.$;
	asideStripe = window.Stripe;
	window.Stripe, window.$ = {};
	window.Stripe = jasmine.createSpyObj('Stripe', ['setPublishableKey']);
	jqueryform = jasmine.createSpyObj('form', ['submit', 'show']);
	window.$ = jasmine.createSpy("$").andReturn(jqueryform);
	creditcardVerifier.register_callbacks();
    });

    afterEach( function () {
	window.Stripe = asideStripe;
	window.$ = aside$;
	asideStripe, aside$, jqueryform = {};
    });

    it('registers the formSubmissionHandler to handle "payment-form"\'s submit event', function () {
	expect(jqueryform.submit).toHaveBeenCalledWith( creditcardVerifier.formSubmissionHandler );
    });
});

/*I think that a test of the form 'event' argument should have property 'fraggle' should be included
with the integration tests.*/
describe('The form submission handler', function () { 
    var asideStripe, aside$, jqueryform, HTMLformSpy, button;
    beforeEach(function () {
	aside$ = window.$;
	asideStripe = window.Stripe;
	window.Stripe, window.$ = {};
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

describe('the Stripe response handler', function (){
    var aside$, fakejQuery, jqueryform, button, fakeFind, errorfieldSpy, fakeAppend, fakeGet, domnodeSpy;
    beforeEach(function () {
	aside$ = window.$;
	jqueryform = jasmine.createSpyObj('form', ['find', 'append', 'get']);
	fakejQuery = function ( jtarget ) {
	    if ( jtarget === '#payment-form' ) {
		return jqueryform
	    } else if ( jtarget === '<input type="hidden" name="stripeToken" />' ) {
		return domnodeSpy;
	    } 
	};
	window.$ = jasmine.createSpy("$").andCallFake(fakejQuery);

	fakeFind = function (target) {
	    if ( target === '.payment-errors'){
		errorfieldSpy = jasmine.createSpyObj('errorfield', ['text']);
		return errorfieldSpy
	    } else if (target === 'button'){
		button = jasmine.createSpyObj('button', ['prop']);
		return button
	    } 
	};
	jqueryform.find.andCallFake(fakeFind);
    });
    afterEach(function () {
	window.$ = aside$;
	aside$ = fakejQuery = jqueryform = button = fakeFind = errorfieldSpy = fakeAppend = fakeGet = domnodeSpy = {};
    });

    describe('when it receives an error in the response', function () {
	var mockerror, mockresponse;
	beforeEach(function () {
	    mockerror = { message : 'An error.'};
	    mockresponse = { error : mockerror};
	});
	afterEach(function () {
	    mockerror = mockerror.message = mockresponse = mockresponse.message = {};
	});

	it('finds the .payment-errors DOM object', function () {
	    //expect(jqueryform).toBeDefined();
	    creditcardVerifier.stripeResponseHandler( jasmine.createSpy("status"), mockresponse);
	    expect(mockresponse.error).toBeDefined();
	    expect(jqueryform.find.calls[0].args[0]).toEqual('.payment-errors');

	});
	it('sets that objects text attribute to the error message', function () {
	    creditcardVerifier.stripeResponseHandler( jasmine.createSpy("status"), mockresponse);
	    expect(errorfieldSpy.text).toHaveBeenCalledWith('An error.');
	});
	it('finds the button that submitted the form', function () {
	    creditcardVerifier.stripeResponseHandler( jasmine.createSpy("status"), mockresponse);
	    expect(jqueryform.find.calls[1].args[0]).toEqual('button');
	});
	it('activates the form submission button', function () {
	    creditcardVerifier.stripeResponseHandler( jasmine.createSpy("status"), mockresponse);
	    expect(button.prop).toHaveBeenCalledWith('disabled', false);
	});
    });
    
    describe('when it does _not_ receive an error', function() {
	var mockerror, mockresponse, appendable, submitter;
	beforeEach(function () {
	    mockresponse = { id : 'response_token'};
	    submitter = jasmine.createSpyObj('getreturn', ['submit']);
	    jqueryform.get.andReturn(submitter);
	    domnodeSpy = jasmine.createSpyObj('jqnode', ['val']);
	    appendable = jasmine.createSpy('appendable');
	    domnodeSpy.val.andReturn(appendable);

	});
	afterEach(function () {
	    mockresponse = mockresponse.id = appendable = submitter = {};
	});
	it('assigns the response id to a local variable', function () {
	    creditcardVerifier.stripeResponseHandler( jasmine.createSpy("status"), mockresponse);
	    expect(mockresponse.id).toBe('response_token');
	});
	it("creates a jquery with argument '<input type=\"hidden\" name=\"stripeToken\" />'", function () {
	    creditcardVerifier.stripeResponseHandler( jasmine.createSpy("status"), mockresponse);
	    expect(window.$).toHaveBeenCalledWith('<input type="hidden" name="stripeToken" />');
	});
	it("sets the value of that jquery object to the value obtained from the response id", function () {
	    creditcardVerifier.stripeResponseHandler( jasmine.createSpy("status"), mockresponse);
	    expect(domnodeSpy.val).toHaveBeenCalledWith('response_token');
	});
	it('appends the jquery with the token value to the submission form', function () {
	    creditcardVerifier.stripeResponseHandler( jasmine.createSpy("status"), mockresponse);
	    expect(jqueryform.append).toHaveBeenCalledWith(appendable);
	});
	it('gets the first element of the form', function () {
	    creditcardVerifier.stripeResponseHandler( jasmine.createSpy("status"), mockresponse);
	    expect(jqueryform.get).toHaveBeenCalledWith(0);
	});
	it('invokes the form\'s submit action', function () {
	    creditcardVerifier.stripeResponseHandler( jasmine.createSpy("status"), mockresponse);
	    expect(jqueryform.get).toHaveBeenCalledWith(0);
	    expect(submitter.submit).toHaveBeenCalled();
	});
    });
});