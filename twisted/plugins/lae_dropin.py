
from twisted.application.service import ServiceMaker

s4_subscription_manager = ServiceMaker(
    "S4 Subscription Manager",
    "lae_automation.subscription_manager",
    "A stateful service for recording and exposing S4 subscriptions.",
    "s4-subscription-manager",
)

s4_subscription_converger = ServiceMaker(
    "S4 Subscription Converger",
    "lae_automation.subscription_converger",
    "A stateless convergence loop for synchronizing Kubernetes configuration with the S4 subscription database.",
    "s4-subscription-converger",
)
