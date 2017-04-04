{ pkgs ? import <nixpkgs> {}
, lae ? import ./lae.nix {} }:

pkgs.dockerTools.buildImage {
  name = "leastauthority/subscription-manager";
  fromImage = lae.s4-common-image;
  config = {
    Cmd =
      [
        "/bin/dash"
        "-c"
        (pkgs.lib.strings.concatStringsSep " " [
          "/app/env/bin/twist" "s4-subscription-manager"
          "--domain" "$(cat /app/k8s_secrets/domain)"
          "--state-path" "/app/data/subscriptions"
          "--listen-address" "tcp:8000"
        ])
      ];
    ExposedPorts = {
      "8000/tcp" = {};
    };
    WorkingDir = "/app/run";
    Volumes = {
      "/app/data" = {};
    };
  };
}
