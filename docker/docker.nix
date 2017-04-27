{ pkgs ? import <nixpkgs> {} }:

rec {
  pyDerivation = args@{
    name,
    builder,
    contents,
  }:
    let
      result = pkgs.stdenv.mkDerivation rec {
        inherit name;

	nativeBuildInputs = [
	  pkgs.python27
	];

        referencePath = "references";
	exportReferencesGraph = [ referencePath contents ];

	realBuilder = "${pkgs.python27}/bin/python";
	args = [ builder ];
	builderHash = builtins.hashString "sha256" (builtins.readFile builder);
      };
    in
      [ result ];

  # Build a Docker image tar.gz which can be loaded into a Docker daemon.
  buildImage = args@{
    # Image name.
    name,

    # Files to put on the image (a set of derivations).
    contents,
  }:

    let
      baseName = baseNameOf name;
      result = pyDerivation {
        name = "docker-image-${baseName}.tar.gz";
	builder = ./build-docker-image.py;
	contents = builtins.attrValues contents;
      };
    in
      result;
}
