{
  description = "twesterhout/hdf5-hs: high-level bindings to HDF5";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = inputs:
    let
      inherit (inputs.nixpkgs) lib;
      haskell-overlay = self: super: {
        haskell = super.haskell // {
          packageOverrides = lib.composeManyExtensions [
            super.haskell.packageOverrides
            (hself: hsuper: {
              hdf5-hs = hself.callCabal2nix "hdf5-hs" ./. {
                hdf5 = super.hdf5;
                hdf5_hl = super.hdf5;
              };
            })
          ];
        };
      };

      pkgs-for = system: import inputs.nixpkgs {
        inherit system;
        overlays = [ haskell-overlay ];
      };
    in
    {
      overlays.default = haskell-overlay;

      packages = inputs.flake-utils.lib.eachDefaultSystemMap (system:
        with pkgs-for system; {
          inherit haskell;
          default = haskellPackages.hdf5-hs;
        });

      devShells = inputs.flake-utils.lib.eachDefaultSystemMap (system:
        {
          default = with pkgs-for system; haskellPackages.shellFor {
            packages = ps: [ ps.hdf5-hs ];
            withHoogle = true;
            nativeBuildInputs = with haskellPackages; [
              cabal-install
              cabal-fmt
              fourmolu
              haskell-language-server
              nil
              nixpkgs-fmt
            ];
          };
        });
    };
}
