
import sys
from lae_automation.monitor import read_serverinfo
from lae_automation.server import notify_zenoss

if len(sys.argv) < 4:
    print "Usage: python initialize_zenoss.py EC2INFOFILE ZENOSSSERVERIP ZENOSSKEYSPATH"
    print "Happy notifying the Zenoss server!"
    sys.exit(1)



pathtoserverinfo = sys.argv[1]
zenoss_IP = sys.argv[2]
zenoss_privkey_path = sys.argv[3]

EC2proptuples = read_serverinfo(pathtoserverinfo)

for proptuple in EC2proptuples:
    EC2pubIP = proptuple[2]
    notify_zenoss(EC2pubIP, zenoss_IP, zenoss_privkey_path)
