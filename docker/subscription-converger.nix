{ pkgs ? import <nixpkgs> {}
, lae ? import ./lae.nix {} }:

pkgs.dockerTools.buildImage {
  name = "leastauthority/subscription-converger";
  fromImage = lae.s4-common-image;
  config = {
    Env = lae.s4-common-image.buildArgs.config.Env;
    EntryPoint = ["/bin/dash" "-c"];
    Cmd =
      [
        (pkgs.lib.strings.concatStringsSep " " [
          "twist" "s4-subscription-converger"
          "--domain" "$(cat /app/k8s_secrets/domain)"
          "--kubernetes-namespace" "$(cat /var/run/secrets/kubernetes.io/serviceaccount/namespace)"
          "--aws-access-key-id-path" "/app/k8s_secrets/aws.id"
          "--aws-secret-access-key-path" "/app/k8s_secrets/aws.key"
          "--introducer-image" "\${LAE_S4_TAHOE_INTRODUCER_IMAGE}"
          "--storageserver-image" "\${LAE_S4_TAHOE_STORAGESERVER_IMAGE}"
          "--endpoint" "\${LAE_S4_SUBSCRIPTION_MANAGER_URL}"
          "--k8s-service-account"
        ])
      ];
    WorkingDir = "/app/run";
  };
}
