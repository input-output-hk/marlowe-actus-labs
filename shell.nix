let
  pkgs = import <nixpkgs> {};
  nodejs = pkgs."nodejs-10_x";
  easyPS = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "15fcd398449b3dd725c0145c298dda541908adc9";
    sha256 = "94mSh+OvkXDUB3oE1DRrUFeS1ZAZC0zCrodXjs7sqLs=";
  }) { inherit pkgs; };
  version = "v0.1.0";
in
  pkgs.stdenv.mkDerivation {
    propagatedBuildInputs = [
      easyPS.spago
      easyPS."purs-0_15_2"
      easyPS.purescript-language-server
      easyPS.pscid
      easyPS.purs-tidy
      easyPS.pulp

      pkgs.dhall
      pkgs.haskell-language-server
      pkgs.nodejs
      pkgs.pkg-config
      pkgs.python27
      pkgs.python37
      pkgs.unzip
    ];
    name = "noname";
    shellHook = "export PATH=$PATH:./node_modules/.bin/:./bin";
  }
