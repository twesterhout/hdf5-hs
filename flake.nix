{
  description = "twesterhout/hdf5-hs: bindings to HDF5";

  nixConfig = {
    extra-experimental-features = "nix-command flakes";
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    with builtins;
    let
      inherit (inputs.nixpkgs) lib;
      pkgs = import inputs.nixpkgs { inherit system; };

      src = inputs.nix-filter.lib {
        root = ./.;
        include = [
          "app"
          "cbits"
          "example"
          "src"
          "test"
          "hdf5-hs.cabal"
          "LICENSE"
          "README.md"
        ];
      };

      haskellPackagesOverride = ps:
        ps.override
          {
            overrides = self: super: {
              hdf5-hs = self.callCabal2nix "hdf5-hs" src {
                hdf5 = pkgs.hdf5;
                hdf5_hl = pkgs.hdf5;
              };
            };
          };

      outputsFor =
        { haskellPackages
        , name
        , package ? ""
        , ...
        }:
        let
          ps = haskellPackagesOverride haskellPackages;
        in
        {
          packages."${name}" = ps.${package} or ps;
          devShells."${name}" = ps.shellFor {
            packages = ps: with ps; [ hdf5-hs ];
            # withHoogle = true;
            nativeBuildInputs = with pkgs; with ps; [
              # Building and testing
              cabal-install
              # Language servers
              haskell-language-server
              nil
              # Formatters
              fourmolu
              cabal-fmt
              nixpkgs-fmt
              # Previewing markdown files
              python3Packages.grip
              # For debugging Halide
              # gcc
              # zlib
              # gdb
            ];
            shellHook = ''
              export LD_LIBRARY_PATH=${pkgs.hdf5}/lib:$LD_LIBRARY_PATH
            '';
          };
          formatter = pkgs.nixpkgs-fmt;
        };
    in
    foldl' (acc: conf: lib.recursiveUpdate acc (outputsFor conf)) { }
      (lib.mapAttrsToList (name: haskellPackages: { inherit name haskellPackages; })
        (lib.filterAttrs (_: ps: ps ? ghc) pkgs.haskell.packages) ++ [
        {
          haskellPackages = pkgs.haskellPackages;
          name = "defaultGhc";
        }
        {
          haskellPackages = pkgs.haskellPackages;
          name = "default";
          package = "hdf5-hs";
        }
      ])
  );
}
