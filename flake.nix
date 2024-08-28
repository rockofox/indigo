{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils/main";
    flake-compat = {
      url = "github:edolstra/flake-compat/master";
      flake = false;
    };
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.flake-compat.follows = "flake-compat";
    };
  };

  description = "";

  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    let
      overlay = se: su: {
        haskellPackages = su.haskellPackages.override {
          overrides = hse: _hsu: {
            "indigo" = hse.callCabal2nix "indigo" ./. { };
          };
        };
        indigo = se.haskell.lib.justStaticExecutables se.haskellPackages.indigo;
      };
    in
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };
      in
      {
        checks = {
          pre-commit-check = inputs.pre-commit-hooks.lib.${system}.run {

            src = ./.;
            hooks = {
              hlint.enable = true;
              nixpkgs-fmt.enable = true;
              # nix-linter.enable = true;
              # statix.enable = true;
              fourmolu.enable = true;
              # cabal-fmt.enable = true;
              shellcheck.enable = true;
            };
          };
        };
        defaultPackage = pkgs.indigo;
        devShell = pkgs.haskellPackages.shellFor {
          packages = p: [ p."indigo" ];
          buildInputs = [
            pkgs.haskellPackages.haskell-language-server
            pkgs.haskellPackages.cabal-install
            pkgs.haskellPackages.ghcid
            pkgs.haskellPackages.fourmolu
            pkgs.haskellPackages.hlint
            pkgs.wasmtime
            pkgs.wabt
            pkgs.wizer
            pkgs.qbe
          ];
          withHoogle = false;
          inherit (inputs.self.checks.${system}.pre-commit-check) shellHook;
        };
      });
}
