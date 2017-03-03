{ pkgs ? import <nixpkgs> {} }:

let
  filepath = pkgs.python27Packages.buildPythonPackage rec {
    name = "${pname}-${version}";
    pname = "filepath";
    version = "0.1";

    buildInputs = [ pkgs.python27Packages.tox ];

    src = pkgs.fetchurl {
      url = "https://pypi.python.org/packages/eb/0f/3c5ebbfd6fa6904181ecece2b6737d797a033479a193984e7e84c1d84185/${name}.tar.gz";
      sha256 = "15wf9krsbjzsllw3h1wx3rwy9lzy8hr1jgjiisg0ipqqn7bgw3cb";
    };

    checkPhase = ''
    # XXX I can't find the tox.ini
    '';
  };

  pem = pkgs.python27Packages.buildPythonPackage rec {
    name = "${pname}-${version}";
    pname = "pem";
    version = "16.1.0";

    buildInputs = [ pkgs.python27Packages.tox ];

    src = pkgs.fetchurl {
      url = "https://pypi.python.org/packages/4f/06/cf51103b48532565adb42f562389d364e2075947c5b1cba79fdc7eae4e8d/${name}.tar.gz";
      sha256 = "0w5i2ywcrjhdv7wjpm9519adcr87braskkbwxs9b6pbfrllvmx2b";
    };
  };

  extras100 = pkgs.python27Packages.extras.override rec {
    name = "${pname}-${version}";
    pname = "extras";
    version = "1.0.0";
    src = pkgs.fetchurl {
      url = "mirror://pypi/e/extras/${name}.tar.gz";
      sha256 = "0khvm08rcwm62wc47j8niyl6h13f8w51c8669ifivjdr23g3cbhk";
    };
  };

  testtools220 = pkgs.python27Packages.testtools.override rec {
    name = "${pname}-${version}";
    pname = "testtools";
    version = "2.2.0";
    propagatedBuildInputs =
      with pkgs.python27Packages;
      [ pbr python_mimeparse extras100 fixtures300 lxml unittest2 ];

    src = pkgs.fetchurl {
      url = "mirror://pypi/t/testtools/${name}.tar.gz";
      sha256 = "042qlqymy9m40w30fg3gj20wahmac9jphnsfwb8f8k3fg9h0dxl0";
    };
    patches = [];
  };

  pyrsistent0_12_0 = pkgs.python27Packages.pyrsistent.override rec {
    name = "pyrsistent-0.12.0";

    src = pkgs.fetchurl {
      url = "mirror://pypi/p/pyrsistent/${name}.tar.gz";
      sha256 = "1k7kjj1nyzyk1jfiawlg16s1nsmypwmfqsq9yc3iba1m6jq9rq9p";
    };
  };

  eliot = pkgs.python27Packages.buildPythonPackage rec {
    name = "eliot-${version}";
    version = "0.12.0";

    propagatedBuildInputs =
      with pkgs.python27Packages;
      [ six zope_interface pyrsistent0_12_0 ];

    src = pkgs.fetchurl {
      url = "mirror://pypi/e/eliot/${name}.tar.gz";
      sha256 = "07k9lbn3aym05h69rw1xiyjh4wp9swcvay3z2w0f1pqab5h0q8xs";
    };
  };

  pyopenssl16_2_0 = pkgs.python27Packages.buildPythonPackage rec {
    name = "pyopenssl-${version}";
    version = "16.2.0";

    src = pkgs.fetchurl {
      url = "mirror://pypi/p/pyOpenSSL/pyOpenSSL-${version}.tar.gz";
      sha256 = "0vji4yrfshs15xpczbhzhasnjrwcarsqg87n98ixnyafnyxs6ybp";
    };

    preCheck = ''
      sed -i 's/test_set_default_verify_paths/noop/' tests/test_ssl.py
    '';

    checkPhase = ''
      runHook preCheck
      export LANG="en_US.UTF-8";
      py.test;
      runHook postCheck
    '';

    buildInputs =
    with pkgs.python27Packages;
    [ pkgs.openssl pytest pkgs.glibcLocales ];

    propagatedBuildInputs =
    with pkgs.python27Packages;
    [ cryptography pyasn1 idna ];
  };

  service-identity = pkgs.python27Packages.service-identity.override {
    propagatedBuildInputs = with pkgs.python27Packages; [
      characteristic pyasn1 pyasn1-modules pyopenssl16_2_0 idna attrs
    ];
  };

  foolscap = pkgs.python27Packages.foolscap.override {
    propagatedBuildInputs =
    with pkgs.python27Packages;
    [ mock twisted pyopenssl16_2_0 service-identity ];
  };

  tahoe_lafs = pkgs.python27Packages.buildPythonPackage rec {
    name = "${pname}-${version}";
    pname = "tahoe-lafs";
    version = "1.11.0";

    src = pkgs.fetchurl {
      url = "https://pypi.python.org/packages/73/9e/53757536146aa197ff2310c90152487b3dc286d1dedd0c522eb51287d7e7/tahoe-lafs-1.11.0.tar.bz2";
      sha256 = "0hrp87rarbmmpnrxk91s83h6irkykds3pl263dagcddbdl5inqdi";
    };

    buildInputs = [ pkgs.python27Packages.tox ];

    propagatedBuildInputs =
      with pkgs.python27Packages; [
        pyasn1
        pyasn1-modules
        simplejson
        dateutil
        zfec
        pycrypto
        pyopenssl16_2_0
        pycryptopp
        service-identity
        twisted
        foolscap
        nevow
      ];

      checkPhase = ''
      # XXX
      # Lots of stuff fails. :(
      '';

  };
  txaws021post5 = pkgs.python27Packages.buildPythonPackage rec {
    name = "${pname}-${version}";
    pname = "txAWS";
    version = "0.2.1.post5";

    src = pkgs.fetchurl {
      url = "https://tahoe-lafs.org/deps/txAWS-0.2.1.post5.tar.gz";
      sha256 = "1mwxpkr3ivsq35cp6s84f0yfmavyskqdf9drhvk5jvnmi6xfppq2";
    };

    propagatedBuildInputs =
      with pkgs.python27Packages;
      [ dateutil twisted ];
  };
  fixtures300 = pkgs.python27Packages.fixtures.override rec {
    name = "fixtures-3.0.0";
    src = pkgs.fetchurl {
      url = "mirror://pypi/f/fixtures/${name}.tar.gz";
      sha256 = "1vxj29bzz3rd4pcy51d05wng9q9dh4jq6wx92yklsm7i6h1ddw7w";
    };
  };
  stripe = pkgs.python27Packages.buildPythonPackage rec {
    name = "${pname}-${version}";
    pname = "stripe";
    version = "1.41.1";

    # Tests require network connectivity and there's no easy way to disable
    # them. ~ C.
    doCheck = false;

    src = pkgs.fetchurl {
      url = "mirror://pypi/s/${pname}/${name}.tar.gz";
      sha256 = "0zvffvq933ia5w5ll6xhx2zgvppgc6zc2mxhc6f0kypw5g2fxvz5";
    };

    buildInputs = with pkgs.python27Packages; [ unittest2 mock ];
    propagatedBuildInputs = with pkgs.python27Packages; [ requests ];

    meta = {
      homepage = "https://github.com/stripe/stripe-python";
      description = "Stripe Python bindings";
      license = pkgs.stdenv.lib.licenses.mit;
    };
  };
in
  pkgs.python27Packages.buildPythonPackage rec {
    name = "s4-${version}";
    version = "1.0.0";

    buildInputs =
      with pkgs.python27Packages;
      [ testtools220 hypothesis mock fixtures300 pelican ];

    propagatedBuildInputs =
      with pkgs.python27Packages; [
        pem
        pyopenssl16_2_0
        filepath
        twisted
        eliot
        stripe
        Fabric
        attrs
        txaws021post5
        tahoe_lafs
      ];

    src = /leastauthority.com;

    checkPhase = ''
      trial lae_util lae_site lae_automation
    '';

    meta = {
      license = pkgs.stdenv.lib.licenses.gpl2;
    };
  }
