{ pkgs ? import <nixpkgs> {}
, lae ? import ./lae.nix {} }:

pkgs.dockerTools.buildImage {
  name = "leastauthority/grid-router";
  fromImage = lae.s4-common-image;
  config = {
    Env = lae.s4-common-image.buildArgs.config.Env;
    EntryPoint = ["/bin/dash" "-c"];
    Cmd = [
      (pkgs.lib.strings.concatStringsSep " " [
        "/bin/twist" "s4-grid-router"
	"--k8s-service-account"
	"--kubernetes-namespace"
        "$(cat /var/run/secrets/kubernetes.io/serviceaccount/namespace)"
      ])
    ];
  };
}
