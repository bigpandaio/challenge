{ stdenv, fetchurl }:

stdenv.mkDerivation rec {
  name = "generator";
  version = "1.0";

  src =
    if builtins.currentSystem == "x86_64-linux" then
      fetchurl {
        url    = "https://s3-us-west-1.amazonaws.com/bp-interview-artifacts/generator-linux-amd64";
        sha256 = "1446rbjw2ql3z1rximwxpqkr1wshdyl1z8awc9wkwa59ikgbwzpq";
      }
    else if builtins.currentSystem == "x86_64-darwin" then
      fetchurl {
        url    = "https://s3-us-west-1.amazonaws.com/bp-interview-artifacts/generator-macosx-amd64";
        sha256 = "0dgrqm23vmmc8wymglbh47cl09fhwg0jciq0ayc8gdjp93rkkbkx";
      }
    else
      abort "Unsupported platform";

  phases = "installPhase";

  installPhase = ''
    mkdir -p $out/bin
    cp ${src} $out/bin/${name}
    chmod +x $out/bin/${name}
  '';
}
