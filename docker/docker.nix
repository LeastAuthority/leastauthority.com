{ pkgs ? import <nixpkgs> {} }:

rec {
  pyDerivation = args@{
    name,
    builder,
    contents,
  }:
    let
      result = pkgs.stdenv.mkDerivation {
        inherit name;

	nativeBuildInputs = [
	  pkgs.python27
	];

	contents = contents;

	realBuilder = "${pkgs.python27}/bin/python";
	args = [ builder ];
	builderHash = builtins.hashString "sha256" (builtins.readFile builder);
      };
    in
      [ result ];

  # Build a single layer which can be composed into a Docker image .tar.gz by
  # buildImage.
  buildLayer = name: contents:
    let
      baseName = baseNameOf name;
      result = pyDerivation {
        name = "docker-layer-${baseName}";
	builder = ./build-docker-layer.py;
	contents = contents;
      };
    in
      result;

  # Build a Docker image tar.gz which can be loaded into a Docker daemon.
  buildImage = args@{
    # Image name.
    name,

    # Files to put on the image (a set of derivations).
    contents,
  }:

    let
      baseName = baseNameOf name;
      layers = builtins.attrValues (pkgs.lib.mapAttrs buildLayer contents);
      result = pyDerivation {
        name = "docker-image-${baseName}.tar.gz";
	builder = ./build-docker-image.py;
	contents = layers;
      };
    in
      result;
}
