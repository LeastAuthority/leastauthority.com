"use strict";


describe('The subscription signup creditcardVerifier', function () {
    //Global Variables Used to Store References to components external to the unit under test
    var aside$, asideStripe;
    //The jquery constructor, all calls to this produce jquery objects and are replaced with spies.
    var jquery$spy;
    //Declare Spies which replace jquery objects
    var ccformspy, use_pgpspy, pageloadspy; // definitions are specific to specs

    beforeEach(function (){
        aside$ = window.$;
        asideStripe = window.Stripe;
        //Dereference Actual objects to isolate the unit under test.
        window.Stripe, window.$ = {};
        jquery$spy = function (value) {
            if ( value === "#use_pgp" ) {
                return use_pgpspy;
            } else if ( value === "#payment-form" ) {
                return ccformspy; // instances are declared in the invoking scope
            } else if ( value === "#loading" ) {
                return pageloadspy;
            }
        }
        spyOn(window, '$').andCallFake(jquery$spy);
    });
    afterEach(function (){
        //Re-reference actual objects, from their set-aside references
        window.Stripe = asideStripe;
        window.$ = aside$;
        //Dereference spies to eliminate test specific state.
        aside$, asideStripe = {};
        jquery$spy = {};
        ccformspy, use_pgpspy, pageloadspy = {};
    });
    xdescribe('The initialize_page callback', function () {
        //Declare variables needed to isolate and monitor the unit under test.
        var asideregister_callbacks;
        beforeEach(function () {
            //Set up spies to record interractions between the unit under test and its context.
            ccformspy = jasmine.createSpyObj('form', ['show']);
            use_pgpspy = jasmine.createSpyObj('#use_pgp', ['prop']);
            asideregister_callbacks = creditcardVerifier.register_callbacks
            spyOn(creditcardVerifier, 'register_callbacks');
            pageloadspy = jasmine.createSpyObj('loadspy', ['hide'])
            //Invoke the unit under test.
            creditcardVerifier.initialize_page();
        });

        afterEach( function () {
            creditcardVerifier.register_callbacks = asideregister_callbacks;
            asideregister_callbacks = {};
        });
        it('encapsulates DOM objects into JQuery', function () {
            expect(window.$).toBeDefined();
            expect(window.$).toHaveBeenCalledWith('#payment-form');
            expect(window.$).toHaveBeenCalledWith('#loading');
        });
        it('hides the #loading element, implying the page is renderable in the served browser', function () {
            expect(pageloadspy.hide).toHaveBeenCalled();
        });
        it('displays the #payment_form, proving JS is interpreted and the browser can render the page', function () {
            expect(ccformspy.show).toHaveBeenCalled();
        });
    });

    describe('The register_callbacks callback', function () {
        //Declare variables needed to isolate and monitor the unit under test.
        var formsubmissionhandlerspy;
        beforeEach(function () {
            window.Stripe = jasmine.createSpyObj('Stripe', ['setPublishableKey']);
            formsubmissionhandlerspy = jasmine.createSpy('formsubspy');
            creditcardVerifier.formSubmissionHandler = formsubmissionhandlerspy;
            ccformspy = {
                submit: function(submithandler){},
            };
            spyOn(ccformspy, 'submit');
            use_pgpspy = {
                click: function(clickhandler){},
            };
            spyOn(use_pgpspy, 'click');
            //Invoke the unit under test.
            creditcardVerifier.register_callbacks(ccformspy);
        });

        afterEach( function () {
            //Dereference spies to eliminate test specific state.
            formsubmissionhandlerspy = {};
        });
        it('sets the publishable stripe key to identify LeastAuth to Stripe during requests', function () {
            expect(window.Stripe.setPublishableKey).toHaveBeenCalledWith('pk_test_IBiTH5UtEo2kB10eb1OSsv0w');
        });
        xit('registers the formSubmissionHandler to handle "payment-form"\'s submit event', function () {
            expect(ccformspy_instance.submit).toHaveBeenCalledWith( formsubmissionhandlerspy );
        });
        xit('registers the use_pgp as the handler for the #use_pgp element click event', function () {
            expect(use_pgpspy.click).toHaveBeenCalledWith(asideuse_pgp);
        });
    });
/*
    describe('The form submission handler', function () { 
        //Declare variables needed to isolate and monitor the unit under test.
        var aside$, asideStripe, ccformspy, jquery$spy, button;
        beforeEach(function () {
            //Set actual objects that interract with unit under test 'aside'
            aside$ = window.$;
            asideStripe = window.Stripe;
            //Dereference Actual objects to isolate the unit under test.
            window.Stripe, window.$ = {};
            //Set up spies to record interractions between the unit under test and its context.
            ccformspy = jasmine.createSpyObj('form', ['find']);
            jquery$spy = function (value) {return ccformspy;}
            spyOn(window,"$").andCallFake(jquery$spy);
            button = jasmine.createSpyObj('button', ['prop']);
            ccformspy.find.andReturn(button);
            window.Stripe = jasmine.createSpyObj('Stripe', ['createToken']);
            //Invoke the unit under test.
            creditcardVerifier.formSubmissionHandler( jasmine.createSpy( "click" ) );
        });

        afterEach( function () {
            //Re-reference actual objects, from their set-aside references
            window.Stripe = asideStripe;
            window.$ = aside$;
            //Dereference spies to eliminate test specific state.
            aside$, asideStripe, ccformspy, jquery$spy, button = {};
        });
        it('finds the form submission button', function () {
            expect(ccformspy.find).toHaveBeenCalledWith('button');
        }); 
        it('disables the form submission button', function () {
            expect(button.prop).toHaveBeenCalledWith('disabled', true);
        });
        it('registers the response handler inside Stripe.createToken', function () {
            expect(Stripe.createToken).toHaveBeenCalledWith( 
                ccformspy, creditcardVerifier.stripeResponseHandler 
            );
        });
    });

    describe('The use_pgp input "checkbox" handler for the #use_pgp element',function(){
        //Declare variables needed to isolate and monitor the unit under test.
        var aside$, asideStripe, use_pgpspy, pgpspanspy, jquery$spy, pgpssinstance, pgpcbspyinstance;
        beforeEach(function () {
            //Set actual objects that interract with unit under test 'aside'
            aside$ = window.$;
            asideStripe = window.Stripe;
            //Dereference Actual objects to isolate the unit under test.
            window.Stripe, window.$ = {};
            //Set up spies to record interractions between the unit under test and its context.
            pgpspanspy = function () {
                this.show = function (){};
                this.hide = function (){};
            };
        });

        afterEach( function () {
            //Re-reference actual objects, from their set-aside references
            window.Stripe = asideStripe;
            window.$ = aside$;
            //Dereference spies to eliminate test specific state.
            asideStripe, aside$, use_pgpspy, pgpspanspy, jquery$spy, pgpssinstance, pgpcbspyinstance = {};
        });
        describe('when the box has been checked',function (){
            beforeEach(function(){
                jquery$spy = function (value) {
                    if ( value === '#use_pgp' ) {
                        pgpcbspyinstance = new use_pgpspy(true);
                        spyOn(pgpcbspyinstance, 'prop').andCallThrough();
                        return pgpcbspyinstance;
                    } else if ( value === "#pgp" ) {
                        pgpssinstance = new pgpspanspy();
                        spyOn(pgpssinstance, 'show');
                        spyOn(pgpssinstance, 'hide');
                        return pgpssinstance;
                    }
                }
                spyOn(window, "$").andCallFake(jquery$spy);
                //Invoke the unit under test.
                creditcardVerifier.use_pgp();
            });
            it('calls "$(\'#pgp\').show();" because "checked" is true', function(){
                expect(pgpcbspyinstance.prop()).toBe(true);
                expect(pgpssinstance.show).toHaveBeenCalled();
            });
            it('does _NOT_ call "$(\'#pgp\').hide();" when "checked" is true', function(){
                expect(pgpssinstance.hide).not.toHaveBeenCalled();
            });
        });
        describe('when the box has _NOT_ been checked',function (){
            beforeEach(function(){
                jquery$spy = function (value) {
                    if ( value === '#use_pgp' ) {
                        pgpcbspyinstance = new use_pgpspy(false);
                        spyOn(pgpcbspyinstance, 'prop').andCallThrough();
                        return pgpcbspyinstance;
                    } else if ( value === "#pgp" ) {
                        pgpssinstance = new pgpspanspy();
                        spyOn(pgpssinstance, 'show');
                        spyOn(pgpssinstance, 'hide');
                        return pgpssinstance;
                    }
                }
                spyOn(window, "$").andCallFake(jquery$spy);
                //Invoke the unit under test.
                creditcardVerifier.use_pgp();
            });
            it('does _NOT_ call "$(\'#pgp\').show();" because "checked" is false', function(){
                expect(pgpcbspyinstance.prop()).toBe(false);
                expect(pgpssinstance.show).not.toHaveBeenCalled();
            });
            it('calls "$(\'#pgp\').hide();" because "checked" is false', function(){
                expect(pgpcbspyinstance.prop()).toBe(false);
                expect(pgpssinstance.hide).toHaveBeenCalled();
            });
        });
    });

    describe('the Stripe response handler', function (){
        //Declare variables needed to isolate and monitor the unit under test.
        var aside$, asideuse_pgp, ccformspy, use_pgpspy, checkedspy, jquerynodespy, errorfieldspy, button, jQueryspy, findspy;
        var errormessagespy, striperesponsespy;
        beforeEach(function () {
            //Set actual objects that interract with unit under test 'aside'
            aside$ = window.$;
            asideuse_pgp = creditcardVerifier.use_pgp;
            //Dereference Actual objects to isolate the unit under test.
            window.$ = {};
            creditcardVerifier.use_pgp = {};
            //Define spy objects
            ccformspy = jasmine.createSpyObj('form', ['find', 'append', 'get']);
            ccformspy.jqnodes = [];
            ccformspy.append.andCallFake(function (jqnode) {ccformspy.jqnodes.push(jqnode)});
            use_pgpspy = { prop: function (status) {return checkedspy} };
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
        });
        afterEach(function () {
            window.$ = aside$;
            creditcardVerifier.use_pgp = asideuse_pgp;
            aside$, ccformspy, use_pgpspy, checkedspy, jquerynodespy, errorfieldspy, button, jQueryspy = {}
            findspy, errormessagespy, striperesponsespy = {};
        });

        describe('when it receives an error in the response', function () {
            beforeEach(function () {
                //Define spy objects
                errormessagespy = { message : 'An error.'};
                striperesponsespy = { error : errormessagespy};
                //Define spy functions
                jQueryspy = function ( jtarget ) {
                    if ( jtarget === '#payment-form' ) {
                        return ccformspy
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
                ccformspy.find.andCallFake(findspy);

            });
            afterEach(function () {
                //Dereference spies to eliminate test specific state.
                errormessagespy.message, striperesponsespy.error = {};
            });

            it('finds the .payment-errors DOM object', function () {
                creditcardVerifier.stripeResponseHandler( jasmine.createSpy("status"), striperesponsespy);
                expect(striperesponsespy.error).toBeDefined();
                expect(ccformspy.find.calls[0].args[0]).toEqual('.payment-errors');

            });
            it('sets that objects text attribute to the error message', function () {
                creditcardVerifier.stripeResponseHandler( jasmine.createSpy("status"), striperesponsespy);
                expect(errorfieldspy.text).toHaveBeenCalledWith('An error.');
            });
            it('finds the button that submitted the form', function () {
                creditcardVerifier.stripeResponseHandler( jasmine.createSpy("status"), striperesponsespy);
                expect(ccformspy.find.calls[1].args[0]).toEqual('button');
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
                jQueryspy = function ( jtarget ) {
                    if ( jtarget === '#payment-form' ) {
                        return ccformspy
                    } else if ( jtarget === '#use_pgp' ) {
                        return use_pgpspy; 
                    } else if ( jtarget === '<input type="hidden" name="pgp_pubkey">' ) {
                        return new jquerynodespy();
                    } else if ( jtarget === '<input type="hidden" name="stripeToken">' ) {
                        return new jquerynodespy();
                    } else if ( jtarget === '#pgp_pubkey_textarea' ) {
                        return "PGP PUBLIC KEY INFO";
                    }
                };
                //Assign spy functions
                spyOn(window, "$").andCallFake(jQueryspy);

                //ccformspy.append
                submitterspy = jasmine.createSpyObj('getreturn', ['submit']);
                ccformspy.get.andReturn(submitterspy);
            });
            afterEach(function () {
                striperesponsespy.id, submitterspy = {};
            });
            describe('when a PGP key is _submitted', function() {
                beforeEach(function (){
                    //Define spy object
                    checkedspy = false;
                    creditcardVerifier.stripeResponseHandler( jasmine.createSpy("status"), striperesponsespy );
                });
                afterEach(function () {
                    checkedspy = {};
                });
                it('will append a jqnode with value \"\" to the form', function () {
                    expect(ccformspy.append.calls[0].args[0].val()).toBe('');
                    expect(ccformspy.jqnodes[0].val()).toBe('');
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
                    expect(ccformspy.append.calls[0].args[0].val()).toBe('');
                    expect(ccformspy.jqnodes[0].val()).toBe('');
                });
            });

            it('assigns the response id to a local variable', function () {
                creditcardVerifier.stripeResponseHandler( jasmine.createSpy("status"), striperesponsespy);
                expect(striperesponsespy.id).toBe('response_token');
            });
            
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
                expect(ccformspy.append).toHaveBeenCalledWith(jquerynodespy);
            });
            it('gets the first element of the form', function () {
                creditcardVerifier.stripeResponseHandler( jasmine.createSpy("status"), striperesponsespy);
                expect(ccformspy.get).toHaveBeenCalledWith(0);
            });
            it('invokes the form\'s submit action', function () {
                creditcardVerifier.stripeResponseHandler( jasmine.createSpy("status"), striperesponsespy);
                expect(ccformspy.get).toHaveBeenCalledWith(0);
                expect(submitterspy.submit).toHaveBeenCalled();
            });
        });
    })*/
});

