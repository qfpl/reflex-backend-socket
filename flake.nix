{
  description = "reflex-backend-socket";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      nixpkgs = import inputs.nixpkgs { inherit system; };
      package = nixpkgs.haskellPackages.callPackage
        ./reflex-backend-socket.nix
        { };
      ghc8107 = nixpkgs.haskell.packages.ghc8107;
      ghc922 = nixpkgs.haskell.packages.ghc922.override {
        overrides = final: prev: with nixpkgs.haskell.lib; {
          patch = doJailbreak prev.patch;
        };
      };
    in
    {
      defaultPackage = package;
      devShell = package.env.overrideAttrs (oldAttrs: {
        buildInputs = oldAttrs.buildInputs ++ [ nixpkgs.cabal-install ];
      });
      devShells = {
        ghc8107 = ghc8107.callPackage ./reflex-backend-socket.nix { };
        ghc922 = ghc922.callPackage ./reflex-backend-socket.nix { };
      };
    }
  );
}
