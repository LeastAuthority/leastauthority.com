
from lae_automation.route53 import get_route53_client
from txaws.service import AWSCredentials, AWSServiceRegion

def main(reactor):
    aws = AWSServiceRegion(AWSCredentials())
    route53 = get_route53_client(reactor, aws)
    return route53.list_resource_record_sets(zone_id="Z2T2TSJ409GHZ9", identifier="", maxitems="", name="", type="A")


from twisted.internet.task import react
react(main)

