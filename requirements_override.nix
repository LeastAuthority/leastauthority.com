{ pkgs, python }:
  let
    drv_named = name: drv:
      (builtins.parseDrvName drv.name).name != "${python.__old.python.libPrefix}-${name}";
    remove = name: drvs: builtins.filter (drv_named name) drvs;

  in
    # remove Twisted from Automat.propagatedBuildInputs
    # remove Twisted from incremental.propagatedBuildInputs
    # remove fixtures from testtools.propagatedBuildInputs
    # remove pyutil from zbase32 propagatedBuildInputs

    # remove unittest2 from
    # mock oauthlib testtools

    # replace pbr in propagatedBuildInputs for
    # fixtures mock testtools

    self: super: {

        "pbr" = python.overrideDerivation super."pbr" (old: {
          doCheck = false;
        });

        "setuptools-scm" = python.mkDerivation {
          name = "setuptools-scm-1.15.6";
          src = pkgs.fetchurl { url = "https://pypi.python.org/packages/03/6d/aafdd01edd227ee879b691455bf19895091872af7e48192bea1758c82032/setuptools_scm-1.15.6.tar.gz"; sha256 = "49ab4685589986a42da85706b3311a2f74f1af567d39fee6cb1e088d7a75fb5f"; };
          doCheck = false;
          buildInputs = [ ];
          propagatedBuildInputs = [ ];
          meta = with pkgs.stdenv.lib; {
            homepage = "https://github.com/pypa/setuptools_scm/";
            license = licenses.mit;
            description = "the blessed package to manage your versions by scm tags";
          };
        };

      "Automat" = python.overrideDerivation super."Automat" (old: {
        propagatedBuildInputs = [
          self."m2r"
          self."attrs"
          self."six"
          self."pbr"
          self."setuptools-scm"
        ];
        propagatedNativeBuildInputs = [];
      });

      "incremental" = python.overrideDerivation super."incremental" (old: {
        propagatedBuildInputs = [
        ];
        propagatedNativeBuildInputs = [
        ];
      });

      "fixtures" = python.overrideDerivation super."fixtures" (old: rec {
        # nativeBuildInputs = old.nativeBuildInputs ++ [ pkgs.pythonPackages.testtools ];
        # patchPhase = ''
        #   sed -i -e 's/, "testtools (>=0.9.22)"//'
        # '';
      });

      "testtools" = python.overrideDerivation super."testtools" (old: rec {
        propagatedNativeBuildInputs = remove "fixtures" old.propagatedNativeBuildInputs;
	# nativeBuildInputs = old.nativeBuildInputs ++ [ self."fixtures" ];
      });


      "linecache2" = python.overrideDerivation super."linecache2" (old: {
        propagatedBuildInputs = old.propagatedBuildInputs ++ [ self."pbr" ];
      });

      "zbase32" = python.overrideDerivation super."zbase32" (old: {
        propagatedBuildInputs = [
        ];
        propagatedNativeBuildInputs = [
        ];
      });

      "mock" = python.overrideDerivation super."mock" (old: rec {
        propagatedBuildInputs = [
          self."unittest2"
          self."Jinja2"
          self."Pygments"
          self."funcsigs"
          self."pbr"
          self."six"
        ];
        propagatedNativeBuildInputs = propagatedBuildInputs;
      });

      "unittest2" = python.overrideDerivation super."unittest2" (old: rec {
        patchPhase = ''
          sed -i -e 's/version=VERSION/version="1.1.0+lae-1.0"/' setup.py
        '';
      });

      "oauthlib" = python.overrideDerivation super."oauthlib" (old: {
        propagatedBuildInputs = [
          self."unittest2"
          self."blinker"
          self."cryptography"
          self."mock"
        ];
      });
    }
