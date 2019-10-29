{ mkDerivation, base, bytestring, containers, lens, mtl, network
, reflex, reflex-basic-host, semialign, semigroupoids, stdenv, stm
, these, witherable
}:
mkDerivation {
  pname = "reflex-backend-socket";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring lens mtl network reflex semialign semigroupoids stm
    these
  ];
  executableHaskellDepends = [
    base bytestring containers lens network reflex reflex-basic-host
    witherable
  ];
  description = "Reflex bindings for TCP sockets";
  license = stdenv.lib.licenses.bsd3;
}
