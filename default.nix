{ reflex-platform ? import ./nix/reflex-platform.nix
, compiler   ? "ghc"
} :
let

  pkgs = reflex-platform.nixpkgs.pkgs;
  ghc = reflex-platform.${compiler};

  modifiedHaskellPackages = ghc.override {
    overrides = self: super: {
      reflex-basic-host = self.callHackageDirect {
        pkg = "reflex-basic-host";
        ver = "0.2";
        sha256 = "10xi8gn7mbw4v5xgphs3mfh24am2vzh04c466if88ibs3mlgsdvy";
      } {};
    };
  };

  drv = modifiedHaskellPackages.callPackage ./reflex-backend-socket.nix {};
in
  drv
