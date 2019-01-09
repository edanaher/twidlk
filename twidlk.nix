{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "twidlk";
  version = "0.0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  description = "Twiddler multitool";
  license = stdenv.lib.licenses.gpl3;
}
