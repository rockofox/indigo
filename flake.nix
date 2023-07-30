{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils/main";
    flake-compat = {
      url = "github:edolstra/flake-compat/master";
      flake = false;
    };
  };

  description = "";

  outputs = { self, nixpkgs, flake-utils, ... }:
    let
      overlay = se: su: {
        haskellPackages = su.haskellPackages.override {
          overrides = hse: _hsu: {
            "indigo" = hse.callCabal2nix "indigo" ./. { };
          };
        };
        indigo =
          se.haskell.lib.justStaticExecutables
            se.haskellPackages.indigo;
      };
    in
    { inherit overlay; } // flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; overlays = [ overlay ]; };
      in
      {
        defaultPackage = pkgs.indigo;
        devShell = pkgs.haskellPackages.shellFor {
          packages = p: [ p."indigo"  ];
          buildInputs = [
            pkgs.haskellPackages.haskell-language-server
            pkgs.haskellPackages.cabal-install
            pkgs.haskellPackages.ghcid
            pkgs.haskellPackages.ormolu
            pkgs.haskellPackages.hlint
            pkgs.wasmtime
            pkgs.wabt
          ];
          # shellHook = ''
          #   alias wasi-cc=$HOME/Downloads/wasi-sdk-20.0/bin/clang
          #   alias wasi-ld=$HOME/Downloads/wasi-sdk-20.0/bin/wasm-ld
          # '';
          withHoogle = false;
        };
      }
    );
}
