{ mkDerivation, base, binary, bytestring, containers, dependent-map
, dependent-sum, lens, mtl, network, reflex, reflex-basic-host
, reflex-binary, stdenv, stm, these
}:
mkDerivation {
  pname = "reflex-server-socket";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base binary bytestring containers dependent-map dependent-sum lens
    mtl network reflex reflex-basic-host reflex-binary stm these
  ];
  executableHaskellDepends = [
    base binary containers mtl network reflex reflex-basic-host
    reflex-binary
  ];
  license = stdenv.lib.licenses.bsd3;
}
