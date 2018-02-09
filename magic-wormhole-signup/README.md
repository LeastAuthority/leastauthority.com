# magic-wormhole-signup

This is the magic-wormhole-based signup service for S4.
It is responsible for initially creating magic-wormhole codes for new subscriptions.
Also, for maintaining and operating the sending side of the corresponding magic-wormhole.

## architecture

1. The signup service polls the subscription manager.
2. For any subscriptions it finds with a magic-wormhole code, it opens a magic-wormhole.
3. For each magic-wormhole thus opened, it will send the subscription details over it.
4. Once subscription details have been transmitted, the signup service removes the wormhole code from the subscription manager.

The signup web server is typically responsible for populating the subscription manager.
When it receives a new signup request, it creates the new subscription.
The subscription manager populates new subscriptions with the random wormhole code this service eventually finds.

This design allows this signup service to be restarted without ruining any subscription attempts currently in progress.
