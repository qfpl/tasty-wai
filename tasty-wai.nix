{ mkDerivation, base, bytestring, http-types, stdenv, tasty, wai
, wai-extra
}:
mkDerivation {
  pname = "tasty-wai";
  version = "0.1.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring http-types tasty wai wai-extra
  ];
  testHaskellDepends = [ base http-types tasty wai ];
  description = "Test 'wai' endpoints via Test.Tasty";
  license = stdenv.lib.licenses.bsd3;
}
