
from twisted.application.service import ServiceMaker

s4_subscription_manager = ServiceMaker(
    "S4 Subscription Manager",
    "lae_automation.subscription_manager",
    "A stateful service for recording and exposing S4 subscriptions.",
    "s4-subscription-manager",
)
