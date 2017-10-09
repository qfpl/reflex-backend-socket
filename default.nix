{ reflex-platform ? import ./reflex-platform.nix
, compiler   ? "ghc"
} :
let

  pkgs = reflex-platform.nixpkgs.pkgs;
  ghc = reflex-platform.${compiler};
  sources = {
    reflex-basic-host = import ./reflex-basic-host.nix;
  };

  modifiedHaskellPackages = ghc.override {
    overrides = self: super: {
      reflex-basic-host = self.callPackage sources.reflex-basic-host {};
    };
  };

  drv = modifiedHaskellPackages.callPackage ./reflex-server-socket.nix {};
in
  drv
