{ pkgs ? import <nixpkgs> {} }:

# The idea here is to generate a derivation representing a layer per
# derivation (or, since there is a limit of around 126 layers in an image, a
# layer per N derivations if the number of derivations is very large).  An
# image can then be constructed fairly cheaply by selecting and assembling the
# layer-derivations corresponding to the necessary software.  Docker requires
# layers to be stacked in a particular order but the layer-derivations should
# stack in any order to produce the same result (since they're disjoint
# /nix/store paths).  Then slap on a layer with a nix env and a Docker
# container config.
#
# Much of the build logic is currently in a Python builder that gets invoked
# by the expressions here.  However, a basic problem with this approach is
# that it's not straightforward to access the closure of a derivation ("the
# necessary software") from a Nix expression.  We can access it from the
# builder easily (this is what exportReferencesGraph is for).  But it's not
# easy to allow a builder to dynamically generate multiple outputs (as it
# needs to do for the layer-derivations it wants to build).
#
# So this doesn't currently work.

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
