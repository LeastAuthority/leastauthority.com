"use strict";


describe('The initialize_page callback', function () {
    //Declare variables needed to isolate and monitor the unit under test.
    var aside$, asideStripe, jqueryformspy, loadspy, jquery$spy;
    beforeEach(function () {
	//Set actual objects that interract with unit under test 'aside'
	aside$ = window.$;
	asideStripe = window.Stripe;
	//Dereference Actual objects to isolate the unit under test.
	window.Stripe, window.$ = {};
	//Set up spies to record interractions between the unit under test and its context.
	jqueryformspy = jasmine.createSpyObj('form', ['show']);  
	loadspy = { hide: function() {}};
	jquery$spy = function (value) {
	    if ( value === "#payment-form" ) {
		return jqueryformspy;
	    } else if ( value === "#loading" ) {
		return loadspy;
	    } 
	}
	spyOn(loadspy, 'hide');
	spyOn(window, '$').andCallFake(jquery$spy);
	spyOn(creditcardVerifier, 'register_callbacks');
	//Invoke the unit under test.
	creditcardVerifier.initialize_page();
    });

    afterEach( function () {
	//Re-reference actual objects, from their set-aside references
	window.Stripe = asideStripe;
	window.$ = aside$;
	//Dereference spies to eliminate test specific state.
	aside$, asideStripe, jqueryformspy, loadspy, jquery$spy = {};
    });

    it('encapsulates DOM objects into JQuery', function () {
	expect(window.$).toHaveBeenCalledWith('#payment-form');
	expect(window.$).toHaveBeenCalledWith('#loading');
    });
    
    it('hides the #loading element, implying the page is renderable in the served browser', function () {
	expect(loadspy.hide).toHaveBeenCalled();
    });

    it('displays the #payment_form, proving it JS is interpreted and the browser can render the page', function () {
	expect(jqueryformspy.show).toHaveBeenCalled();
    });

});

describe('The register_callbacks callback', function () {
    //Declare variables needed to isolate and monitor the unit under test.
    var aside$, asideStripe, asidefsh, asideuse_pgp, pgpcheckboxspy, jquery$spy;
    var	formsubmissionhandlerspy, use_pgpspy, jqueryformspy;
    beforeEach(function () {
	//Set actual objects that interract with unit under test 'aside'
	aside$ = window.$;
	asideStripe = window.Stripe;
	asidefsh = creditcardVerifier.formSubmissionHandler;
	asideuse_pgp = creditcardVerifier.use_pgp;
	//Dereference Actual objects to isolate the unit under test.
	window.Stripe, window.$ = {};
	//Set up spies to record interractions between the unit under test and its context.
	pgpcheckboxspy = {click: function ( pgpusehandler ) {},}
	jquery$spy = function (value) {
	    if ( value === "#use_pgp" ) {
		return pgpcheckboxspy;
	    }  
	}
	spyOn(window, '$').andCallFake(jquery$spy);
	spyOn(pgpcheckboxspy, 'click')
	formsubmissionhandlerspy = jasmine.createSpy('formsubspy');
	creditcardVerifier.formSubmissionHandler = formsubmissionhandlerspy;
	use_pgpspy = jasmine.createSpy('use_pgp_method_spy');
	creditcardVerifier.use_pgp = use_pgpspy;
	window.Stripe = jasmine.createSpyObj('Stripe', ['setPublishableKey']);
	jqueryformspy = jasmine.createSpyObj('form', ['submit']);
	//Invoke the unit under test.
	creditcardVerifier.register_callbacks( jqueryformspy );
    });

    afterEach( function () {
	//Re-reference actual objects, from their set-aside references
	window.Stripe = asideStripe;
	window.$ = aside$;
	creditcardVerifier.formSubmissionHandler = asidefsh;
	creditcardVerifier.use_pgp = asideuse_pgp;
	//Dereference spies to eliminate test specific state.
	aside$, asideStripe, asidefsh, asideuse_pgp, pgpcheckboxspy, jquery$spy,+
	formsubmissionhandlerspy, use_pgpspy, jqueryformspy = {};
    });

    it('sets the publishable stripe key to identify LeastAuth to Stripe during requests', function () {
	expect(window.Stripe.setPublishableKey).toHaveBeenCalledWith('pk_test_IBiTH5UtEo2kB10eb1OSsv0w');
    });

    it('registers the formSubmissionHandler to handle "payment-form"\'s submit event', function () {
	expect(jqueryformspy.submit).toHaveBeenCalledWith( formsubmissionhandlerspy );
    });

    it('registers the use_pgp as the handler for the #use_pgp element click event', function () {
	expect(pgpcheckboxspy.click).toHaveBeenCalledWith( use_pgpspy );
    });

});

describe('The form submission handler', function () { 
    //Declare variables needed to isolate and monitor the unit under test.
    var aside$, asideStripe, jqueryformspy, jquery$spy, button;
    beforeEach(function () {
	//Set actual objects that interract with unit under test 'aside'
	aside$ = window.$;
	asideStripe = window.Stripe;
	//Dereference Actual objects to isolate the unit under test.
	window.Stripe, window.$ = {};
	//Set up spies to record interractions between the unit under test and its context.
	jqueryformspy = jasmine.createSpyObj('form', ['find']);
	jquery$spy = function (value) {return jqueryformspy;}
	spyOn(window,"$").andCallFake(jquery$spy);
	button = jasmine.createSpyObj('button', ['prop']);
	jqueryformspy.find.andReturn(button);
	window.Stripe = jasmine.createSpyObj('Stripe', ['createToken']);
	//Invoke the unit under test.
	creditcardVerifier.formSubmissionHandler( jasmine.createSpy( "click" ) );
    });

    afterEach( function () {
	//Re-reference actual objects, from their set-aside references
	window.Stripe = asideStripe;
	window.$ = aside$;
	//Dereference spies to eliminate test specific state.
	aside$, asideStripe, jqueryformspy, jquery$spy, button = {};
    });

    it('finds the form submission button', function () {
	expect(jqueryformspy.find).toHaveBeenCalledWith('button');
    }); 

    it('disables the form submission button', function () {
	expect(button.prop).toHaveBeenCalledWith('disabled', true);
    }); 

    it('registers the response handler inside Stripe.createToken', function () {
	expect(Stripe.createToken).toHaveBeenCalledWith( 
	    jqueryformspy, creditcardVerifier.stripeResponseHandler 
	);
    });
});

describe('The use_pgp input "checkbox" handler for the #use_pgp element',function(){
    //Declare variables needed to isolate and monitor the unit under test.
    var aside$, asideStripe, jquerycheckbox, jquery$spy;
    beforeEach(function () {
	//Set actual objects that interract with unit under test 'aside'
	aside$ = window.$;
	asideStripe = window.Stripe;
	//Dereference Actual objects to isolate the unit under test.
	window.Stripe, window.$ = {};
	//Set up spies to record interractions between the unit under test and its context.
	jquerycheckbox = {fadeToggle: function(seconds) {return true}};
	spyOn(jquerycheckbox, 'fadeToggle');
	jquery$spy = function (value) {return jquerycheckbox;}
	spyOn(window,"$").andCallFake(jquery$spy);
	//Invoke the unit under test.
	creditcardVerifier.use_pgp( jasmine.createSpy( "checked" ) );
    });

    afterEach( function () {
	//Re-reference actual objects, from their set-aside references
	window.Stripe = asideStripe;
	window.$ = aside$;
	//Dereference spies to eliminate test specific state.
	asideStripe, aside$, jquerycheckbox, jquery$spy = {};
    });

    it('toggles the visibility of the #pgp text area', function(){
	expect(jquerycheckbox.fadeToggle).toHaveBeenCalled();
    });
});

describe('the Stripe response handler', function (){
    //Declare variables needed to isolate and monitor the unit under test.
    var aside$, asideuse_pgp, jqueryformspy, pgpcheckboxspy, checkedspy, jquerynodespy, errorfieldspy, button, jQueryspy, findspy;
    var errormessagespy, striperesponsespy;
    beforeEach(function () {
	//Set actual objects that interract with unit under test 'aside'
	aside$ = window.$;
	asideuse_pgp = creditcardVerifier.use_pgp;
	//Dereference Actual objects to isolate the unit under test.
	window.$ = {};
	creditcardVerifier.use_pgp = {};
	//Define spy objects
	jqueryformspy = jasmine.createSpyObj('form', ['find', 'append', 'get']);
	jqueryformspy.jqnodes = [];
	jqueryformspy.append.andCallFake(function (jqnode) {jqueryformspy.jqnodes.push(jqnode)});
	pgpcheckboxspy = { prop: function (status) {return checkedspy} };
	//jquerynodespy = jasmine.createSpyObj('jqnode', ['val']);
	jquerynodespy = function ( ) {
	    this.value = '';
	    this.val = function ( value = null ) {
		if ( value === null ) {
		    return this.value;
		} else {
		    var jqns = new jquerynodespy();
		    jqns.value = value;
		    return jqns
		}
	    }
	}
	errorfieldspy = jasmine.createSpyObj('errorfield', ['text']);
	button = jasmine.createSpyObj('button', ['prop']);

	//Define spy functions
	jQueryspy = function ( jtarget ) {
	    if ( jtarget === '#payment-form' ) {
		return jqueryformspy
	    } else if ( jtarget === '#use_pgp' ) {
		return pgpcheckboxspy; //Not defined until specific cases!
	    } else if ( jtarget === '<input type="hidden" name="pgp_pubkey">' ) {
		return new jquerynodespy(); 
	    } else if ( jtarget === '<input type="hidden" name="stripeToken">' ) {
		return new jquerynodespy(); 
	    } else if ( jtarget === '#pgp_pubkey_textarea' ) {
		return "PGP PUBLIC KEY INFO"; 
	    } 
	};
	findspy = function (target) {
	    if ( target === '.payment-errors'){
		return errorfieldspy
	    } else if (target === 'button'){
		return button
	    } 
	};
	//Assign spy functions
	spyOn(window, "$").andCallFake(jQueryspy);
	jqueryformspy.find.andCallFake(findspy);
    });
    afterEach(function () {
	window.$ = aside$;
	creditcardVerifier.use_pgp = asideuse_pgp;
	aside$, jqueryformspy, pgpcheckboxspy, checkedspy, jquerynodespy, errorfieldspy, button, jQueryspy = {}
	findspy, errormessagespy, striperesponsespy = {};
    });

    describe('when it receives an error in the response', function () {
	beforeEach(function () {
	    //Define spy objects
	    errormessagespy = { message : 'An error.'};
	    striperesponsespy = { error : errormessagespy};
	});
	afterEach(function () {
	    //Dereference spies to eliminate test specific state.
	    errormessagespy.message, striperesponsespy.error = {};
	});

	it('finds the .payment-errors DOM object', function () {
	    creditcardVerifier.stripeResponseHandler( jasmine.createSpy("status"), striperesponsespy);
	    expect(striperesponsespy.error).toBeDefined();
	    expect(jqueryformspy.find.calls[0].args[0]).toEqual('.payment-errors');

	});
	it('sets that objects text attribute to the error message', function () {
	    creditcardVerifier.stripeResponseHandler( jasmine.createSpy("status"), striperesponsespy);
	    expect(errorfieldspy.text).toHaveBeenCalledWith('An error.');
	});
	it('finds the button that submitted the form', function () {
	    creditcardVerifier.stripeResponseHandler( jasmine.createSpy("status"), striperesponsespy);
	    expect(jqueryformspy.find.calls[1].args[0]).toEqual('button');
	});
	it('activates the form submission button', function () {
	    creditcardVerifier.stripeResponseHandler( jasmine.createSpy("status"), striperesponsespy);
	    expect(button.prop).toHaveBeenCalledWith('disabled', false);
	});
    });

    describe('when it does _not_ receive an error', function() {
        //Declare variables needed to isolate and monitor the unit under test.
	var submitterspy;
	beforeEach(function () {
	    //Define spy objects
	    striperesponsespy = { 
		id : 'response_token',
		error: false
	    };
	    //jqueryformspy.append
	    submitterspy = jasmine.createSpyObj('getreturn', ['submit']);
	    jqueryformspy.get.andReturn(submitterspy);
	});
	afterEach(function () {
	    striperesponsespy.id, submitterspy = {};
	});
	describe('when a PGP key is _NOT_ submitted', function() {
	    beforeEach(function (){
		//Define spy object
		checkedspy = false;
		creditcardVerifier.stripeResponseHandler( jasmine.createSpy("status"), striperesponsespy );
	    });
	    afterEach(function () {
		checkedspy = {};
	    });
	    it('will append a jqnode with value \"\" to the form', function () {
		expect(jqueryformspy.append.calls[0].args[0].val()).toBe('');
		expect(jqueryformspy.jqnodes[0].val()).toBe('');
	    });
	});
	describe('when a PGP key _IS_ submitted', function() {
	    beforeEach(function (){
		//Define spy object
		checkedspy = true;
		creditcardVerifier.stripeResponseHandler( jasmine.createSpy("status"), striperesponsespy );
	    });
	    afterEach(function () {
		checkedspy = {};
	    });
	    it('will append a jqnode with value \"XX\" to the form', function () {
		expect(jqueryformspy.append.calls[0].args[0].val()).toBe('');
		expect(jqueryformspy.jqnodes[0].val()).toBe('');
	    });
	});
/*
	    it('assigns the response id to a local variable', function () {
		creditcardVerifier.stripeResponseHandler( jasmine.createSpy("status"), striperesponsespy);
		expect(striperesponsespy.id).toBe('response_token');
	    });*/
/*
	it("creates a jquery with argument '<input type=\"hidden\" name=\"stripeToken\" />'", function () {
	    creditcardVerifier.stripeResponseHandler( jasmine.createSpy("status"), striperesponsespy);
	    expect(window.$).toHaveBeenCalledWith('<input type="hidden" name="stripeToken" />');
	});
	it("sets the value of that jquery object to the value obtained from the response id", function () {
	    creditcardVerifier.stripeResponseHandler( jasmine.createSpy("status"), striperesponsespy);
	    expect(jquerynodespy.val).toHaveBeenCalledWith('response_token');
	});
	it('appends the jquery with the token value to the submission form', function () {
	    creditcardVerifier.stripeResponseHandler( jasmine.createSpy("status"), striperesponsespy);
	    expect(jqueryformspy.append).toHaveBeenCalledWith(jquerynodespy);
	});
	it('gets the first element of the form', function () {
	    creditcardVerifier.stripeResponseHandler( jasmine.createSpy("status"), striperesponsespy);
	    expect(jqueryformspy.get).toHaveBeenCalledWith(0);
	});
	it('invokes the form\'s submit action', function () {
	    creditcardVerifier.stripeResponseHandler( jasmine.createSpy("status"), striperesponsespy);
	    expect(jqueryformspy.get).toHaveBeenCalledWith(0);
	    expect(submitterspy.submit).toHaveBeenCalled();
	});*/
    });
});