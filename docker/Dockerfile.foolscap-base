#
# This builds an image which is used as the base for foolscap-using
# applications (eg the log gatherer and Tahoe-LAFS).  The anticipated
# name is <leastauthority/foolscap-base>.
#

FROM leastauthority/base

RUN /app/env/bin/pip install foolscap --find-links=https://tahoe-lafs.org/deps
