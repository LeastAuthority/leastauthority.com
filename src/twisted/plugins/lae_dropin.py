
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

s4_grid_router = ServiceMaker(
    "S4 Grid Router",
    "grid_router",
    "A self-configuring proxy/router for per-subscription Tahoe-LAFS access.",
    "s4-grid-router",
)

s4_tahoe_transfer_rate_monitor = ServiceMaker(
    "S4 Tahoe-LAFS Transfer Rate Monitor",
    "lae_util._monitor_tahoe",
    "A service for collecting Tahoe-LAFS transfer rate metrics.",
    "s4-tahoe-lafs-transfer-rate-monitor",
)
