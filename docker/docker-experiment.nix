{ docker ? import ./docker.nix {}
, lae ? import ./lae.nix {} }:

docker.buildImage {
  name = "foo";
  contents = { lae = lae.lae; };
}
