{ mkDerivation, base, bytestring, containers, mtl, network, reflex
, reflex-basic-host, stdenv, stm
}:
mkDerivation {
  pname = "reflex-server-socket";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers mtl network reflex reflex-basic-host stm
  ];
  license = stdenv.lib.licenses.bsd3;
}
