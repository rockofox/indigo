{
  description = "A very basic flake";
  inputs = {
    ghc-wasm-meta.url = "https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/archive/master/ghc-wasm-meta-master.tar.gz";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, flake-utils, ghc-wasm-meta, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };

      in
      with pkgs;
      {
        devShells.default = mkShell {
          buildInputs = [ ghc-wasm-meta.packages.${system}.default ];
        };
      }
    );
}
