{ pkgs ? import <nixpkgs> {} }:

let
  leastauthority = pkgs.callPackage ./lae.nix {
    pkgs = pkgs;
    pythonPackages = pkgs.python27Packages;
  };

  web = { python, lae, pkgs }:
    pkgs.dockerTools.buildImage {
      name = "leastauthority/web";
      runAsRoot = ''
        #!${pkgs.stdenv.shell}
        ${pkgs.dockerTools.shadowSetup}
        groupadd --system web
        useradd --system --gid web --home-dir /app/data web
      '';

      contents = python.buildEnv.override {
        extraLibs = [ pkgs.dash lae ];
	ignoreCollisions = true;
	postBuild = "\${out}/bin/twistd --help > /dev/null";
      };

      config = {
        Cmd =
          let
            port = "8443";
            cert = "certKey=/app/k8s_secrets/website-cert.pem";
            key = "privateKey=/app/k8s_secrets/website-key.pem";
            chain = "extraCertChain=/app/k8s_secrets/website-chain.pem";
          in
            [
	      "/bin/dash" "-c"
	      "/bin/python -u /${python.sitePackages}/lae_site/main.py " +
              "--secure-port=ssl:${port}:${cert}:${key}:${chain} " +
              "--insecure-port=tcp:8080 " +
              "--redirect-to-port=\${S4_SERVICE_PORT_HTTPS_SERVER} " +
              "--signup-furl-path=/app/flapp-data/signup.furl " +
              "--stripe-secret-api-key-path=/app/k8s_secrets/stripe-private.key " +
              "--stripe-publishable-api-key-path=/app/k8s_secrets/stripe-publishable.key " +
              "--site-logs-path=/app/data/logs/sitelogs " +
              "--interest-path=/app/data/emails.csv " +
              "--subscriptions-path=/app/data/subscriptions.csv " +
              "--service-confirmed-path=/app/data/service_confirmed.csv"
            ];
        ExposedPorts = {
          "8443/tcp" = {};
        };
        WorkingDir = "/app/run";
        Volumes = {
          "/app/data" = {};
        };
      };
    };
in
  web {
    python = pkgs.python27;
    lae = leastauthority.lae;
    pkgs = pkgs;
  }

# nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
# nix-channel --update
# nix-build -A pythonFull '<nixpkgs>'
