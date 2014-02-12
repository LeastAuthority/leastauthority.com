"use strict";


describe('The subscription signup creditcardVerifier', function () {
    //Global Variables Used to Store References to components external to the unit under test
    var aside$, asideStripe;
    //The jquery constructor, all calls to this produce jquery objects and are replaced with spies.
    var jquery$spy;
    //Declare Spies which replace jquery objects
    var ccformspy, use_pgpspy, pageloadspy, pgpspanspy, jquerynodespy; // definitions are specific to specs

    beforeEach(function (){
        aside$ = window.$;
        asideStripe = window.Stripe;
        //Dereference Actual objects to isolate the unit under test.
        window.Stripe, window.$ = {};
        jquery$spy = function (jtarget) {
            if (jtarget === "#use_pgp") {
                return use_pgpspy;
            } else if (jtarget === "#payment-form") {
                return ccformspy; // instances are declared in the invoking scope
            } else if (jtarget === "#loading") {
                return pageloadspy;
            } else if (jtarget === "#pgp") {
                return pgpspanspy;
            } else if (jtarget === '<input type="hidden" name="pgp_pubkey">') {
                return jquerynodespy;
            } else if ( jtarget === '<input type="hidden" name="stripeToken">') {
                return jquerynodespy;
            } else if ( jtarget === '#pgp_pubkey_textarea') {
                return jquerynodespy;
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
        ccformspy, use_pgpspy, pageloadspy, pgpspanspy, jquerynodespy = {};
    });
    describe('The initialize_page callback', function () {
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
        var formsubmissionhandlerspy, use_pgpmethodspy, asidefsh, asideuse_pgpmethod;
        beforeEach(function () {
            asidefsh = creditcardVerifier.formSubmissionHandler
            asideuse_pgpmethod = creditcardVerifier.use_pgp
            window.Stripe = jasmine.createSpyObj('Stripe', ['setPublishableKey']);
            formsubmissionhandlerspy = jasmine.createSpy('formsubspy');
            creditcardVerifier.formSubmissionHandler = formsubmissionhandlerspy;
            use_pgpmethodspy = jasmine.createSpy('use_pgp_method');
            creditcardVerifier.use_pgp = use_pgpmethodspy;
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
            creditcardVerifier.formSubmissionHandler = asidefsh;
            creditcardVerifier.use_pgp = asideuse_pgpmethod;
            //Dereference spies to eliminate test specific state.
            formsubmissionhandlerspy, use_pgpmethodspy = {};
        });
        it('sets the publishable stripe key to identify LeastAuth to Stripe during requests', function () {
            expect(window.Stripe.setPublishableKey).toHaveBeenCalledWith('pk_test_IBiTH5UtEo2kB10eb1OSsv0w');
        });
        it('registers the formSubmissionHandler to handle "payment-form"\'s submit event', function () {
            expect(ccformspy.submit).toHaveBeenCalledWith( formsubmissionhandlerspy );
        });
        it('registers the use_pgp as the handler for the #use_pgp element click event', function () {
            expect(use_pgpspy.click).toHaveBeenCalledWith(use_pgpmethodspy);
        });
    });

    describe('The form submission handler', function () { 
        var buttonspy;
        beforeEach(function () {
            buttonspy = jasmine.createSpyObj('button', ['prop']);
            ccformspy = {
                find: function(element){return buttonspy;}
            };
            spyOn(ccformspy, 'find').andCallThrough();
            window.Stripe = jasmine.createSpyObj('Stripe', ['createToken']);
            //Invoke the unit under test.
            creditcardVerifier.formSubmissionHandler( jasmine.createSpy( "click" ) );
        });

        afterEach( function () {
            buttonspy = {};
        });
        it('finds the form submission button', function () {
            expect(ccformspy).toBeDefined();
            expect(ccformspy.find).toBeDefined();
            expect(ccformspy.find).toHaveBeenCalledWith('button');
        }); 
        it('disables the form submission button', function () {
            expect(buttonspy.prop).toHaveBeenCalledWith('disabled', true);
        });
        it('registers the response handler inside Stripe.createToken', function () {
            expect(Stripe.createToken).toHaveBeenCalledWith( 
                ccformspy, creditcardVerifier.stripeResponseHandler 
            );
        });
    });

    describe('The use_pgp method, click handler for the #use_pgp element',function(){
        beforeEach(function () {
            pgpspanspy = {
                show: function (){},
                hide: function (){}
            };
            spyOn(pgpspanspy, 'show')
            spyOn(pgpspanspy, 'hide')
            use_pgpspy = {
                prop: function(checkobject){},
            };
        });
        describe('when the box has been checked',function (){
            beforeEach(function(){
                spyOn(use_pgpspy, 'prop').andReturn(true);
                //Invoke the unit under test.
                creditcardVerifier.use_pgp();
            });
            it('calls "$(\'#pgp\').show();" because "checked" is true', function(){
                expect(use_pgpspy.prop).toHaveBeenCalledWith('checked');
                expect(pgpspanspy.show).toHaveBeenCalled();
            });
            it('does _NOT_ call "$(\'#pgp\').hide();" when "checked" is true', function() {
                expect(use_pgpspy.prop).toHaveBeenCalledWith('checked');
                expect(pgpspanspy.hide).not.toHaveBeenCalled();
            });
        });
        describe('when the box has _NOT_ been checked',function (){
            beforeEach(function(){
                spyOn(use_pgpspy, 'prop').andReturn(false);
                //Invoke the unit under test.
                creditcardVerifier.use_pgp();
            });
            it('does _NOT_ call "$(\'#pgp\').show();" because "checked" is false', function(){
                expect(use_pgpspy.prop).toHaveBeenCalledWith('checked');
                expect(pgpspanspy.show).not.toHaveBeenCalled();
            });
            it('calls "$(\'#pgp\').hide();" because "checked" is false', function(){
                expect(use_pgpspy.prop).toHaveBeenCalledWith('checked');
                expect(pgpspanspy.hide).toHaveBeenCalled();
            });
        });
    });

    describe('the Stripe response handler', function (){
        describe('when it receives an error in the response', function () {
            //Declare variables needed to isolate and monitor the unit under test.
            var errormessagespy, striperesponsespy, errortextspy, buttonspy;
            beforeEach(function () {
                //Define spy objects
                errormessagespy = { message : 'An error.'};
                striperesponsespy = { error : errormessagespy};
                buttonspy = jasmine.createSpyObj('button', ['prop']);
                errortextspy = {
                    text: function(errortxt){}
                };
                spyOn(errortextspy, 'text');
                ccformspy = {
                    find: function(element_id) {
                        if (element_id === ".payment-errors") {
                            return errortextspy;
                        } else if (element_id === "button") {
                            return buttonspy;
                        }
                    }
                };
                spyOn(ccformspy, 'find').andCallThrough();
                creditcardVerifier.stripeResponseHandler(jasmine.createSpy("status"), striperesponsespy);
            });
            afterEach(function () {
                errormessagespy, striperesponsespy, errortextspy, buttonspy = {};
                errormessagespy.message, striperesponsespy.error = {};
            });
            it('finds the .payment-errors DOM object', function () {
                expect(striperesponsespy.error).toBeDefined();
                expect(ccformspy.find.calls[0].args[0]).toEqual('.payment-errors');
            });
            it('sets that objects text attribute to the error message', function () {
                creditcardVerifier.stripeResponseHandler(jasmine.createSpy("status"), striperesponsespy);
                expect(errortextspy.text).toHaveBeenCalledWith('An error.');
            });
            it('finds the button that submitted the form', function () {
                creditcardVerifier.stripeResponseHandler(jasmine.createSpy("status"), striperesponsespy);
                expect(ccformspy.find.calls[1].args[0]).toEqual('button');
            });
            it('activates the form submission button', function () {
                creditcardVerifier.stripeResponseHandler(jasmine.createSpy("status"), striperesponsespy);
                expect(buttonspy.prop).toHaveBeenCalledWith('disabled', false);
            });
        });

        describe('when it does _not_ receive an error', function() {
            //Declare variables needed to isolate and monitor the unit under test.
            var striperesponsespy, submitspy, formnodes;
            beforeEach(function () {
                formnodes = []
                striperesponsespy = {id : 'response_token'};
                submitspy = {
                    submit: function(){},
                };
                spyOn(submitspy, 'submit');
                ccformspy = {
                    get: function(nodenumber){
                        return submitspy;
                    },
                    find: function(){},
                    append: function(newnode){
                        formnodes.push(newnode);
                    },
                };
                spyOn(ccformspy, 'find').andCallThrough();
                spyOn(ccformspy, 'append').andCallThrough();
                spyOn(ccformspy, 'get').andCallThrough();
                jquerynodespy = {
                    val: function() {
                        if (arguments.length === 0) {
                            return "PGP PUBLIC KEY INFO";
                        } else {
                            return arguments[0];
                        }
                    }
                };
                spyOn(jquerynodespy, 'val').andCallThrough();
            });
            afterEach(function () {
                striperesponsespy.id, striperesponsespy, submitspy, formnodes = {};
            });
            describe('when a PGP key _is_ submitted', function() {
                beforeEach(function (){
                    use_pgpspy = {
                        prop: function(checkobject) {return true},
                    };
                    spyOn(use_pgpspy, 'prop').andCallThrough();
                    //Invoke the unit under test.
                    creditcardVerifier.stripeResponseHandler(jasmine.createSpy("status"), striperesponsespy);
                });
                it('two nodes will be appended to the submission form', function () {
                    expect(formnodes.length).toBe(2);
                });
                it('the first node will have value "PGP PUBLIC KEY INFO"', function (){
                    expect(formnodes[0]).toBe("PGP PUBLIC KEY INFO");
                });
                it('the second node have value "response_token"', function (){
                    expect(formnodes[1]).toBe("response_token");
                });
                it('disables the #use_pgp checkbox element', function (){
                    expect(use_pgpspy.prop.calls[1].args[0]).toBe('disabled');
                    expect(use_pgpspy.prop.calls[1].args[1]).toBe(true);
                });
                it('calls the submit function of the first element of the form DOM', function(){
                    expect(ccformspy.get).toHaveBeenCalledWith(0);
                    expect(submitspy.submit).toHaveBeenCalled();
                });
            });
            describe('when a PGP key is _not_ submitted', function() {
                beforeEach(function (){
                    use_pgpspy = {
                        prop: function(checkobject) {return false},
                    };
                    spyOn(use_pgpspy, 'prop').andCallThrough();
                    //Invoke the unit under test.
                    creditcardVerifier.stripeResponseHandler(jasmine.createSpy("status"), striperesponsespy);
                });
                it('two nodes will be appended to the submission form', function () {
                    expect(formnodes.length).toBe(2);
                });
                it('the first node will have value ""', function (){
                    expect(formnodes[0]).toBe("");
                });
                it('the second node have value "response_token"', function (){
                    expect(formnodes[1]).toBe("response_token");
                });
                it('disables the #use_pgp checkbox element', function (){
                    expect(use_pgpspy.prop.calls[1].args[0]).toBe('disabled');
                    expect(use_pgpspy.prop.calls[1].args[1]).toBe(true);
                });
                it('calls the submit function of the first element of the form DOM', function(){
                    expect(ccformspy.get).toHaveBeenCalledWith(0);
                    expect(submitspy.submit).toHaveBeenCalled();
                });
            });
        });
    })
});

