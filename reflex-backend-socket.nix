{ mkDerivation, base, binary, bytestring, containers, dependent-map
, dependent-sum, lens, mtl, network, reflex, reflex-basic-host
, semigroupoids, stdenv, stm, these, transformers, witherable
}:
mkDerivation {
  pname = "reflex-backend-socket";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base binary bytestring containers dependent-map dependent-sum lens
    mtl network reflex reflex-basic-host semigroupoids stm these
    transformers
  ];
  executableHaskellDepends = [
    base binary bytestring containers lens mtl network reflex
    reflex-basic-host witherable
  ];
  description = "Reflex bindings for TCP sockets";
  license = stdenv.lib.licenses.bsd3;
}
