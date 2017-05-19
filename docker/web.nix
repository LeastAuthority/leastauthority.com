{ pkgs ? import <nixpkgs> {}
, lae ? import ./lae.nix {} }:

pkgs.dockerTools.buildImage {
  name = "leastauthority/web";
  fromImage = lae.s4-common-image;
  config = {
    Env = lae.s4-common-image.buildArgs.config.Env;
    EntryPoint = ["/bin/dash" "-c"];
    Cmd =
      let
        port = "8443";
        cert = "certKey=/app/k8s_secrets/website-cert.pem";
        key = "privateKey=/app/k8s_secrets/website-key.pem";
        chain = "extraCertChain=/app/k8s_secrets/website-chain.pem";
      in
        [
          (pkgs.lib.strings.concatStringsSep " " [
            "/bin/python" "-u" "/${pkgs.python27.sitePackages}/lae_site/main.py"
            "--secure-port=ssl:${port}:${cert}:${key}:${chain}"
            "--insecure-port=tcp:8080"
            "--redirect-to-port=\${S4_SERVICE_PORT_HTTPS_SERVER}"
            "--stripe-secret-api-key-path=/app/k8s_secrets/stripe-private.key"
            "--stripe-publishable-api-key-path=/app/k8s_secrets/stripe-publishable.key"
            "--site-logs-path=/app/data/logs/sitelogs"
	    "--subscription-manager=http://subscription-manager/"
          ])
        ];
    ExposedPorts = {
      "8443/tcp" = {};
    };
    WorkingDir = "/app/run";
    Volumes = {
      "/app/data" = {};
    };
  };
}
