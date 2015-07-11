{ mkDerivation, base, base58string, binary, bytestring, hexstring
, hspec, stdenv, text
}:
mkDerivation {
  pname = "bitcoin-types";
  version = "0.9.2";
  src = ./.;
  buildDepends = [
    base base58string binary bytestring hexstring text
  ];
  testDepends = [ base base58string bytestring hexstring hspec ];
  homepage = "http://www.leonmergen.com/opensource.html";
  description = "Provides consistent low-level types used commonly among Bitcoin implementations";
  license = stdenv.lib.licenses.mit;
}
