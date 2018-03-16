{ reflex-platform ? import ./nix/reflex-platform.nix
, compiler   ? "ghc"
} :
let

  pkgs = reflex-platform.nixpkgs.pkgs;
  ghc = reflex-platform.${compiler};

  sources = {
    reflex-basic-host = import ./nix/reflex-basic-host.nix;
    reflex-binary = import ./nix/reflex-binary.nix;
  };

  modifiedHaskellPackages = ghc.override {
    overrides = self: super: {
      reflex-basic-host = self.callPackage sources.reflex-basic-host {};
      reflex-binary = self.callPackage sources.reflex-binary {};
    };
  };

  drv = modifiedHaskellPackages.callPackage ./reflex-server-socket.nix {};
in
  drv
