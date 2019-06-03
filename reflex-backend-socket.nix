{ mkDerivation, base, binary, bytestring, containers, dependent-map
, dependent-sum, lens, mtl, network, reflex, reflex-basic-host
, stdenv, stm, these, transformers
}:
mkDerivation {
  pname = "reflex-backend-socket";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base binary bytestring containers dependent-map dependent-sum lens
    mtl network reflex reflex-basic-host stm these transformers
  ];
  executableHaskellDepends = [
    base binary bytestring containers mtl network reflex
    reflex-basic-host
  ];
  license = stdenv.lib.licenses.bsd3;
}
