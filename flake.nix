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
      };
    in
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };
        wasmGHC = ghc-wasm-meta.packages.${system}.wasm32-wasi-ghc-9_14;
        wasmCabal = ghc-wasm-meta.packages.${system}.wasm32-wasi-cabal-9_14;

        indigoWasm = pkgs.stdenv.mkDerivation {
          pname = "indigo-wasm";
          version = "0.1.0.0";
          src = pkgs.lib.cleanSource ./.;
          nativeBuildInputs = [
            wasmGHC
            wasmCabal
            pkgs.cabal-install
          ];
          __impure = true;
          buildPhase = ''
            export HOME=$TMPDIR
            export CABAL_DIR=$TMPDIR/.cabal
            mkdir -p $CABAL_DIR
            cd wasm_reactor
            wasm32-wasi-cabal update
            wasm32-wasi-cabal build lib:indigo --allow-newer --project-file=cabal.project
          '';
          installPhase = ''
            mkdir -p $out
            cp -r dist-newstyle $out/
          '';
          dontFixup = true;
        };

        indigoWasmReactor = pkgs.stdenv.mkDerivation {
          pname = "indigo-wasm-reactor";
          version = "0.1.0.0";
          src = pkgs.lib.cleanSource ./.;
          nativeBuildInputs = [
            wasmGHC
            wasmCabal
            pkgs.cabal-install
          ];
          buildInputs = [
            indigoWasm
          ];
          __impure = true;
          buildPhase = ''
            export HOME=$TMPDIR
            export CABAL_DIR=$TMPDIR/.cabal
            mkdir -p $CABAL_DIR
            cp -r ${indigoWasm}/dist-newstyle ./dist-newstyle || true
            cd wasm_reactor
            wasm32-wasi-cabal update
            wasm32-wasi-cabal build exe:indigo-wasm-reactor --allow-newer --project-file=cabal.project
          '';
          installPhase = ''
            INDIGO_WASM=$(wasm32-wasi-cabal list-bin exe:indigo-wasm-reactor --allow-newer | tail -n 1)
            mkdir -p $out/bin
            cp "$INDIGO_WASM" $out/bin/indigo-wasm-reactor.wasm
          '';
          dontFixup = true;
        };

        build-wasm-reactor = pkgs.stdenv.mkDerivation {
          pname = "indigo-wasm-reactor";
          version = "0.1.0.0";
          src = pkgs.lib.cleanSource ./.;
          nativeBuildInputs = [
            pkgs.wizer
          ];
          buildInputs = [
            indigoWasmReactor
          ];
          __impure = true;
          buildPhase = ''
            export HOME=$TMPDIR
            export XDG_CACHE_HOME=$TMPDIR/.cache
            mkdir -p $out $XDG_CACHE_HOME
            INDIGO_WASM=${indigoWasmReactor}/bin/indigo-wasm-reactor.wasm
            if [ ! -f "$INDIGO_WASM" ]; then
              echo "Error: Could not find wasm file at $INDIGO_WASM"
              find ${indigoWasmReactor} -type f | head -20
              exit 1
            fi
            cp "$INDIGO_WASM" "$out/indigo-wasm-reactor.wasm"
            ${pkgs.wizer}/bin/wizer --allow-wasi --wasm-bulk-memory true "$out/indigo-wasm-reactor.wasm" -o "$out/indigo-init.wasm"
          '';
          dontFixup = true;
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
              cabal-build-test = {
                enable = true;
                name = "Cabal build and test";
                entry = "${pkgs.writeShellApplication {
                  name = "cabal-build-test";
                  runtimeInputs = [ pkgs.haskellPackages.cabal-install ];
                  text = ''
                    set -e
                    cabal build --ghc-options=-Werror
                    cabal test all
                  '';
                }}/bin/cabal-build-test";
                pass_filenames = false;
              };
            };
          };
        };
        defaultPackage = pkgs.indigo;
        packages = {
          default = pkgs.indigo;
          indigo = pkgs.indigo;
          build-wasm-reactor = build-wasm-reactor;
        };
        devShell = pkgs.haskellPackages.shellFor {
          packages = p: [ p."indigo" ];
          buildInputs = [
            pkgs.haskellPackages.haskell-language-server
            pkgs.haskellPackages.cabal-install
            pkgs.haskellPackages.ghcid
            pkgs.haskellPackages.fourmolu
            pkgs.haskellPackages.hlint
            pkgs.haskellPackages.ghci-dap
            pkgs.haskellPackages.haskell-debug-adapter
            pkgs.wasmtime
            pkgs.wabt
            pkgs.wizer
            ghc-wasm-meta.packages.${system}.default
          ];
          withHoogle = false;
        };
      });
}
