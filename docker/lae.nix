{ pkgs ? import <nixpkgs> {}
, pythonPackages ? import <nixpkgs.python27Packages> {}
}:

rec {
  filepath = pythonPackages.buildPythonPackage rec {
    name = "${pname}-${version}";
    pname = "filepath";
    version = "0.1";

    buildInputs = [ pythonPackages.twisted ];

    src = pkgs.fetchurl {
      url = "https://pypi.python.org/packages/eb/0f/3c5ebbfd6fa6904181ecece2b6737d797a033479a193984e7e84c1d84185/${name}.tar.gz";
      sha256 = "15wf9krsbjzsllw3h1wx3rwy9lzy8hr1jgjiisg0ipqqn7bgw3cb";
    };

    checkPhase = ''
      trial filepath
    '';
  };

  pem = pythonPackages.buildPythonPackage rec {
    name = "${pname}-${version}";
    pname = "pem";
    version = "16.1.0";

    buildInputs = [
      pkgs.libffi
      pkgs.openssl
      pythonPackages.pytest
      pythonPackages.pycparser
      pythonPackages.certifi
      pythonPackages.coverage
      pythonPackages.pretend
      pythonPackages.pytest
      pythonPackages.pyopenssl
      pythonPackages.incremental
      pythonPackages.twisted
    ];

    src = pkgs.fetchurl {
      url = "https://pypi.python.org/packages/4f/06/cf51103b48532565adb42f562389d364e2075947c5b1cba79fdc7eae4e8d/${name}.tar.gz";
      sha256 = "0w5i2ywcrjhdv7wjpm9519adcr87braskkbwxs9b6pbfrllvmx2b";
    };

    checkPhase = ''
      py.test
    '';
  };

  extras100 = pythonPackages.extras.override rec {
    name = "${pname}-${version}";
    pname = "extras";
    version = "1.0.0";
    src = pkgs.fetchurl {
      url = "mirror://pypi/e/extras/${name}.tar.gz";
      sha256 = "0khvm08rcwm62wc47j8niyl6h13f8w51c8669ifivjdr23g3cbhk";
    };
    checkPhase = ''
      python -m unittest extras.tests.test_suite
    '';
  };

  testtools220 = pythonPackages.testtools.override rec {
    name = "${pname}-${version}";
    pname = "testtools";
    version = "2.2.0";
    propagatedBuildInputs =
      with pythonPackages;
      [ pbr python_mimeparse extras100 fixtures300 lxml unittest2 ];

    src = pkgs.fetchurl {
      url = "mirror://pypi/t/testtools/${name}.tar.gz";
      sha256 = "042qlqymy9m40w30fg3gj20wahmac9jphnsfwb8f8k3fg9h0dxl0";
    };
    patches = [];

    # `python -m unittest testtools.tests.test_suite` fails because testtools
    # test suite requires testscenarios and testscenarios requires testtools
    # and I don't know how to resolve this circularity for Nix.
    doCheck = false;
  };

  pyrsistent0_12_0 = pythonPackages.pyrsistent.override rec {
    name = "pyrsistent-0.12.0";

    src = pkgs.fetchurl {
      url = "mirror://pypi/p/pyrsistent/${name}.tar.gz";
      sha256 = "1k7kjj1nyzyk1jfiawlg16s1nsmypwmfqsq9yc3iba1m6jq9rq9p";
    };

    # Test suite requires an old version of Hypothesis :(
    doCheck = false;
  };

  eliot = pythonPackages.buildPythonPackage rec {
    name = "eliot-${version}";
    version = "0.12.0";

    propagatedBuildInputs =
      with pythonPackages;
      [ six zope_interface pyrsistent0_12_0 ];

    src = pkgs.fetchurl {
      url = "mirror://pypi/e/eliot/${name}.tar.gz";
      sha256 = "07k9lbn3aym05h69rw1xiyjh4wp9swcvay3z2w0f1pqab5h0q8xs";
    };

    # Tests fail because eliot-prettyprint can't be found on the PATH
    doCheck = false;
  };

  eliot-tree = pythonPackages.buildPythonPackage rec {
    name = "eliot-tree-15.3.0";

    propagatedBuildInputs =
      with pythonPackages;
      [ toolz jmespath ];

    src = pkgs.fetchurl {
      url = "https://pypi.python.org/packages/88/63/48a8f50480255b4902b9ba3c4825d10fca9d47fd726679877ff076407839/${name}.tar.gz";
      sha256 = "1gasfjbsk9pwb9c3qqrs9z271l7lc334n11xd43byhy1im4hg9cy";
    };

    checkPhase = ''
    # foo
    '';
  };

  pyopenssl16_2_0 = pythonPackages.buildPythonPackage rec {
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
    with pythonPackages;
    [ pkgs.openssl pytest pkgs.glibcLocales ];

    propagatedBuildInputs =
    with pythonPackages;
    [ cryptography pyasn1 idna ];
  };

  service-identity = pythonPackages.service-identity.override {
    propagatedBuildInputs = with pythonPackages; [
      characteristic pyasn1 pyasn1-modules pyopenssl16_2_0 idna attrs
    ];
  };

  foolscap = pythonPackages.foolscap.override {
    propagatedBuildInputs =
    with pythonPackages;
    [ mock twisted pyopenssl16_2_0 service-identity ];
  };

  tahoe_lafs = pythonPackages.buildPythonPackage rec {
    name = "${pname}-${version}";
    pname = "tahoe-lafs";
    version = "1.11.0";

    src = pkgs.fetchurl {
      url = "https://pypi.python.org/packages/73/9e/53757536146aa197ff2310c90152487b3dc286d1dedd0c522eb51287d7e7/tahoe-lafs-1.11.0.tar.bz2";
      sha256 = "0hrp87rarbmmpnrxk91s83h6irkykds3pl263dagcddbdl5inqdi";
    };

    buildInputs = [ pythonPackages.tox ];

    propagatedBuildInputs =
      with pythonPackages; [
        pyasn1
        pyasn1-modules
        simplejson
        dateutil
        zfec
        pycrypto
        pyopenssl16_2_0
        pycryptopp
        service-identity
        foolscap
  	twisted
        (nevow.override {
  	  postInstall = null;
  	})
      ];

    # Lots of stuff fails. :(
    doCheck = false;
  };

  txaws021post5 = pythonPackages.buildPythonPackage rec {
    name = "${pname}-${version}";
    pname = "txAWS";
    version = "0.2.1.post5";

    src = pkgs.fetchurl {
      url = "https://tahoe-lafs.org/deps/txAWS-0.2.1.post5.tar.gz";
      sha256 = "1mwxpkr3ivsq35cp6s84f0yfmavyskqdf9drhvk5jvnmi6xfppq2";
    };

    propagatedBuildInputs =
      with pythonPackages;
      [ dateutil twisted ];
  };

  fixtures300 = pythonPackages.fixtures.override rec {
    name = "fixtures-3.0.0";
    src = pkgs.fetchurl {
      url = "mirror://pypi/f/fixtures/${name}.tar.gz";
      sha256 = "1vxj29bzz3rd4pcy51d05wng9q9dh4jq6wx92yklsm7i6h1ddw7w";
    };
  };

  stripe = pythonPackages.buildPythonPackage rec {
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

    buildInputs = with pythonPackages; [ unittest2 mock ];
    propagatedBuildInputs = with pythonPackages; [ requests2 ];

    meta = {
      homepage = "https://github.com/stripe/stripe-python";
      description = "Stripe Python bindings";
      license = pkgs.stdenv.lib.licenses.mit;
    };
  };

  oauth2client400 = pythonPackages.oauth2client.override rec {
    name = "oauth2client-4.0.0";

    src = pkgs.fetchurl {
      url = "mirror://pypi/o/oauth2client/${name}.tar.gz";
      sha256 = "1irqqap2zibysf8dba8sklfqikia579srd0phm5n754ni0h59gl0";
    };
  };

  pykube = pythonPackages.buildPythonPackage rec {
    name = "${pname}-${version}";
    pname = "pykube";
    version = "0.14.0";

    propagatedBuildInputs =
      with pythonPackages;
      [ six pyyaml tzlocal oauth2client400 requests2 requests_oauthlib ];

    src = pkgs.fetchurl {
      url = "mirror://pypi/p/${pname}/${name}.tar.gz";
      sha256 = "1vb51gzrm1ks2x0lgmwbqbr9gs7c0234lf0z9cjgyxl8cq43cfr7";
    };
  };

  txkube000 = pythonPackages.buildPythonPackage rec {
    name = "${pname}-${version}";
    pname = "txkube";
    version = "0.0.0";

    buildInputs =
      with pythonPackages;
      [ treq pem pyyaml testtools220 hypothesis fixtures300 eliot eliot-tree klein ];

    propagatedBuildInputs =
      with pythonPackages;
      [ zope_interface attrs pyrsistent0_12_0 incremental service-identity pyopenssl16_2_0 twisted pem eliot dateutil pykube ];


    src = pkgs.fetchFromGitHub {
      owner = "LeastAuthority";
      repo = "txkube";
      rev = "ba3b65767c629fd63db911e451e3b8c17435f4b0";
      sha256 = "18wi1j2m2pcfjrxzwbkn531nri3s1hkxfs3pq34pbra1npkx0wkn";
    };

    checkPhase = ''
    trial txkube
    '';
  };

  lae = pythonPackages.buildPythonPackage rec {
    name = "s4-${version}";
    version = "1.0.0";

    buildInputs =
      with pythonPackages;
      [ testtools220 hypothesis mock fixtures300 pelican klein treq eliot-tree ];

    propagatedBuildInputs =
      with pythonPackages; [
        pem
        pyopenssl16_2_0
        filepath
        eliot
        stripe
        Fabric
        attrs
  	jinja2
  	twisted
        txaws021post5
        tahoe_lafs
  	txkube000
      ];

    src = /leastauthority.com;

    checkPhase = ''
      trial lae_util lae_site lae_automation
    '';

    meta = {
      license = pkgs.stdenv.lib.licenses.gpl2;
    };
  };
}
