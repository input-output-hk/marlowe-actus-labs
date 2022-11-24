{
  description = "marlowe-actus-labs";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };

    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, easy-purescript-nix, CHaP, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev:
          # This overlay adds our project to pkgs
          let
            ps = pkgs.callPackage easy-purescript-nix {};
          in {
            codegenProject =
              final.haskell-nix.project' {
                src = ./.;

                compiler-nix-name = "ghc8107";

                # materialized = ./nix/materialized/${ system };

                inputMap = {
                  "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP;
                };
                # This is used by `nix develop .` to open a shell for use with
                # `cabal`, `hlint` and `haskell-language-server`
                shell.tools = {
                   cabal = {};
                   haskell-language-server = {};
                   # hlint = {};
                };
                # Non-Haskell shell tools go here
                shell.buildInputs = with pkgs; [
                  ps."purs-0_15_6"
                  ps.purescript-language-server
                  ps.pscid
                  ps.purs-tidy
                  ps.pulp
                  ps.spago

                  nixpkgs-fmt
                  # pkgs.dhall
                  # pkgs.haskell-language-server
                  # pkgs.nodejs
                  # pkgs.pkg-config
                  # pkgs.python27
                  # pkgs.python37
                  # pkgs.unzip
                ];
              };
            }
          )
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };

      hsPkgs = pkgs.codegenProject.hsPkgs;

      flake = pkgs.codegenProject.flake {};
    in flake // {
      devShells.default = pkgs.codegenProject.shellFor {
        withHoogle = false;
        shellHook = ''
          export PS1="\n\[\033[1;32m\][nix develop:\w]\$\[\033[0m\] "
        '';

        buildInputs = [ ];

        # This triggers compliation and other problems like `plutus-script-utils` compilation:
        # buildInputs = [ hsPkgs.marlowe-cli.components.exes.marlowe-cli ];
      };
      # Built by `nix build .`
      packages.default = flake.packages."codegenProject:exe:codegen";
    });
}
