"use strict";

describe('subcription to stripe', function () {
  it('should be defined', function () {
    expect(creditcardVerifier).toBeDefined();
    expect(creditcardVerifier.stripeResponseHandler).toBeDefined();
    expect(creditcardVerifier.formSubmissionHandler).toBeDefined();
    expect(creditcardVerifier.register_callbacks).toBeDefined();
  });
});
