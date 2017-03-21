# Source me.


node_address() {
    local node_name=$1
    echo $(kubectl get -o json nodes | jp --unquoted "items[?metadata.name == '"${node_name}"'].status.addresses[]|[?type == 'ExternalIP']|[0].address")
}

kubectl_ssh() {
    local node_name=$1
    shift
    ssh admin@$(node_address "${node_name}") "$*"
}
