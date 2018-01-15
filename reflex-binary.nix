let  
  initialNixpkgs = import <nixpkgs> {};

  sources = {
    reflex-binary = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "dalaing";
      repo = "reflex-binary";
      rev = "5f812d8aab0e7559d8e331a21865e81f70b1c67e";
      sha256 = "14n0w04krdrv4qixp783zgp7sz1bsmjsr2p0hcm85hsi93ayl0gj";
    };
  };
in
  sources.reflex-binary

