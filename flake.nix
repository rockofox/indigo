{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', inputs', pkgs, system, ... }:
        {
          # Most simple configuration requires only:
          # haskellProjects.default = { };
          haskellProjects.default = {
            # Haskell dependency overrides go here 
            # overrides = self: super: {
            # };
            # hlsCheck = false;
            # hlintCheck = true;
            packages = {
              funk.root = ./.;
            };
          };
          packages.default = self'.packages.funk;
          devShells.default = with pkgs; [ wasmtime ];
        };
    };
}
