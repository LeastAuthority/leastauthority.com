{ fetchPypi, boto3, certbot, toPythonModule, buildPythonPackage }:
buildPythonPackage rec {
  pname = "certbot-dns-route53";
  version = "0.30.0";
  name = "${pname}-${version}";

  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256:1dp9snp8fy5vs917n62fxq50qxm85xivisn2v8zimclk1wddf7pq";
  };

  propagatedBuildInputs = [ (toPythonModule certbot) boto3 ];
}
