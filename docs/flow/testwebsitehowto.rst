Well, someday it'll be rst...

ssh into theta

(If you need to set up the flappserver, and you will if you're setting up a
new testserver, then consult: /docs/flow/devpaysubmit_to_runflapp.txt.)

If you're not already user website then become "website".

set pwd to "/var/source/leastauthority.com"

This is the "staging repository" see:
"/docs/flow/repoarchitecture.txt" 
to learn what that means and how it relates to the "production" repo which
contains the code backing the live website.

Notice that:  "/var/source/signup.furl" refers to the staging-specific
flappserver.

THETAPORT=8888
Run:

"./runsite.sh --nossl --port=THETAPORT &"

e.g.:

website@domU-12-31-38-06-F8-98:/var/source/leastauthority.com$ ./runsite.sh
--nossl --port=THETAPORT &

Where & launches in the background.

To connect run e.g. the following on your local machine:

ssh -l <USERONTHETA> -f leastauthority.com  -L LOCALPORT/127.0.0.1/THETAPORT -N
