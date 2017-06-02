{ pkgs ? import <nixpkgs> {}
, lae ? import ./lae.nix {} }:


s4-common-image = pkgs.dockerTools.buildImage {
  name = "leastauthority/s4-common";
  config = {
    Env = [
      # pkgs.cacert below provides this file.  The simple ca certificate
      # discovery techniques employed by txAWS and Twisted can't find the
      # certificates in it without a hint like this.
      "SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt"
    ];
  };
  contents = [
    pkgs.cacert
    pkgs.dash
    pkgs.coreutils
    (pkgs.python27.buildEnv.override {
      extraLibs = [ lae.lae ];
      ignoreCollisions = true;
      postBuild = "\${out}/bin/twistd --help > /dev/null";
    })
  ];
};
