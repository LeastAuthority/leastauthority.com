let
  kubeconfig = pkgs: root:
    let
      concatStrings = pkgs.lib.strings.concatStrings;
      intersperse = pkgs.lib.strings.intersperse;
      shellpath = x: concatStrings (intersperse ":" x);
      hasSuffix = pkgs.lib.strings.hasSuffix;
      sort = builtins.sort builtins.lessThan;
      isyaml = (hasSuffix ".yaml");
      listdir = x: map (p: (builtins.toString x) + "/" + p) (builtins.attrNames (builtins.readDir x));
    in
      shellpath (builtins.filter isyaml (listdir root));
in
  { pkgs ? import <nixpkgs> { } }:
  pkgs.stdenv.mkDerivation rec {
    name = "leastauthority-s4";
    buildInputs = [
      # required to build lxml in the virtualenv
      pkgs.libxml2
      pkgs.libxslt
      # required for k8s management, directly and via our tools
      pkgs.kubectl
      # required for secrets management
      pkgs.sops
    ];
    # ValueError: ZIP does not support timestamps before 1980
    SOURCE_DATE_EPOCH = 1559322353;

    # Python bytecode files are such a pain.
    PYTHONDONTWRITEBYTECODE = 1;

    # Tell kubectl where to find our configuration files so it can find our
    # cluster.
    KUBECONFIG = kubeconfig pkgs ~/.kube;

    shellHook = ''
      # Automatically enable the virtualenv
      export PATH=/tmp/virtualenvs/s4/bin:$PATH
    '';
}
