let  
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    reflex-binary-info-pinned = initialNixpkgs.pkgs.lib.importJSON ./reflex-binary.json;
    reflex-binary = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "dalaing";
      repo = "reflex-binary";
      inherit (reflex-binary-info-pinned) rev sha256;
    };
  };
in
  sources.reflex-binary

