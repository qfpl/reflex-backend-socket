{ mkDerivation, base, binary, bytestring, containers, lens, mtl
, network, reflex, reflex-basic-host, reflex-binary, stdenv, stm
, these
}:
mkDerivation {
  pname = "reflex-server-socket";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base binary bytestring containers lens mtl network reflex
    reflex-basic-host reflex-binary stm these
  ];
  license = stdenv.lib.licenses.bsd3;
}
