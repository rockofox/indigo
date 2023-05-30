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
            "prisma" = hse.callCabal2nix "prisma" self { };
          };
        };
        prisma =
          se.haskell.lib.justStaticExecutables
            se.haskellPackages.prisma;
      };
    in
    { inherit overlay; } // flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; overlays = [ overlay ]; };
      in
      {
        defaultPackage = pkgs.prisma;
        devShell = pkgs.haskellPackages.shellFor {
          packages = p: [ p."prisma" ];
          buildInputs = [
            pkgs.haskellPackages.haskell-language-server
            pkgs.haskellPackages.cabal-install
            pkgs.haskellPackages.ghcid
            pkgs.haskellPackages.ormolu
            pkgs.haskellPackages.hlint
            pkgs.wasmtime
            pkgs.wabt
          ];
          withHoogle = false;
        };
      }
    );
}
