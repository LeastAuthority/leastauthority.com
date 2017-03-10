{ pkgs ? import <nixpkgs> {} }:

let
  leastauthority = pkgs.callPackage ./lae.nix {
    pkgs = pkgs;
    pythonPackages = pkgs.python27Packages;
  };

  grid-router = { python, lae, pkgs }:
    pkgs.dockerTools.buildImage {
      name = "leastauthority/grid-router";
      runAsRoot = ''
        #!${pkgs.stdenv.shell}
        ${pkgs.dockerTools.shadowSetup}
        groupadd --system router
        useradd --system --gid router --home-dir /app/data router
      '';
      contents = python.buildEnv.override {
        extraLibs = [ pkgs.dash lae ];
	ignoreCollisions = true;
	postBuild = "\${out}/bin/twistd --help > /dev/null";
      };
      config = {
        Cmd = [
	  "/bin/dash"
	  "-c"
	  (pkgs.lib.strings.concatStringsSep " " [
	    "/bin/twist" "s4-grid-router" "--kubernetes-namespace"
	    "$(cat /var/run/secrets/kubernetes.io/serviceaccount/namespace)"
	  ])
	];
      };
    };
in
  grid-router {
    python = pkgs.python27;
    lae = leastauthority.lae;
    pkgs = pkgs;
  }
