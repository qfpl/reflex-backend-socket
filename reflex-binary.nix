let  
  initialNixpkgs = import <nixpkgs> {};

  sources = {
    reflex-binary = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "dalaing";
      repo = "reflex-binary";
      rev = "720de01a43e0ac3a6a2dd50fc44a879bc64b644e";
      sha256 = "0y5iia2y1pxahk85sjj7zn096d85q7klnwpw3l56map2zgpf5dq0";
    };
  };
in
  sources.reflex-binary

