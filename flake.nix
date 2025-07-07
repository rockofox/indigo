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
    ghc-wasm-meta.url = "https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/archive/master/ghc-wasm-meta-master.tar.gz";
  };

  description = "";

  outputs = inputs@{ self, nixpkgs, flake-utils, ghc-wasm-meta, ... }:
    let
      overlay = se: su: {
        haskellPackages = su.haskellPackages.override {
          overrides = hse: _hsu: {
            "indigo" = hse.callCabal2nix "indigo" ./. { };
          };
        };
        indigo = se.haskell.lib.justStaticExecutables se.haskellPackages.indigo;
        build-wasm-reactor = se.stdenv.mkDerivation {
          pname = "indigo-wasm-reactor";
          version = "0.1.0.0";
          allowNetwork = true;
          src = se.lib.cleanSource ./.;
          nativeBuildInputs = [
            # se.cabal-install
            se.wizer
            ghc-wasm-meta.packages.${se.stdenv.hostPlatform.system}.default
          ];
          buildPhase = ''
            export HOME=$TMPDIR
            export CABAL_DIR=$TMPDIR/.cabal
            mkdir -p $CABAL_DIR
            export CABAL_CONFIG=$CABAL_DIR/config
            cd wasm_reactor
            # Initialize cabal config (no --config-file here)
            # wasm32-wasi-cabal user-config init
            # Use regular cabal to update the package index first
            # cabal update --config-file=$CABAL_CONFIG
            wasm32-wasi-cabal --http-transport=plain-http update
            # Copy the package index to the wasm cabal directory
            cp -r $HOME/.cabal/packages $CABAL_DIR/ || true
            wasm32-wasi-cabal --http-transport=plain-http build exe:indigo-wasm-reactor --allow-newer='base' -f '-ffi'
          '';
          installPhase = ''
            INDIGO_WASM=$(wasm32-wasi-cabal list-bin exe:indigo-wasm-reactor --allow-newer=base | tail -n 1)
            mkdir -p "$out"
            cp "$INDIGO_WASM" "$out/indigo-wasm-reactor.wasm"
            ${se.wizer}/bin/wizer --allow-wasi --wasm-bulk-memory true "$out/indigo-wasm-reactor.wasm" -o "$out/indigo-init.wasm"
          '';
          dontFixup = true;
        };
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
        packages = {
          default = pkgs.indigo;
          indigo = pkgs.indigo;
          build-wasm-reactor = pkgs.build-wasm-reactor;
        };
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
            ghc-wasm-meta.packages.${system}.default
          ];
          withHoogle = false;
          inherit (inputs.self.checks.${system}.pre-commit-check) shellHook;
        };
      });
}
