{ nixpkgs ? import ./nix/nixpkgs.nix
, compiler ? "default"
, doBenchmark ? false
}:

let
  inherit (nixpkgs) pkgs;

  baseHaskellPackages = if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  haskellPackages = baseHaskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib; {
      # Required by reflex
      dependent-map = super.dependent-map_0_3;
      dependent-sum = super.dependent-sum_0_6_2_0;
      monoidal-containers = super.monoidal-containers_0_6;
      witherable = super.callHackage "witherable" "0.3.1" {};

      # Test with new network
      network = super.network_3_1_1_0;

      reflex = enableCabalFlag (unmarkBroken super.reflex) "split-these";
      reflex-basic-host = self.callHackageDirect {
        pkg = "reflex-basic-host";
        ver = "0.2";
        sha256 = "10xi8gn7mbw4v5xgphs3mfh24am2vzh04c466if88ibs3mlgsdvy";
      } {};
    };
  };

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
in
  variant (haskellPackages.callPackage ./reflex-backend-socket.nix {})
