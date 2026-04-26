{
  description = "pomodoro";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        haskellPackages = pkgs.haskellPackages;
      in
      rec
      {
        packages.pomodoro =
          # activateBenchmark
          (haskellPackages.callCabal2nix "pomodoro" ./. {
            # Dependency overrides go here
          });

        defaultPackage = packages.pomodoro;

        devShell =
          let
            scripts = pkgs.symlinkJoin {
              name = "scripts";
              paths = pkgs.lib.mapAttrsToList pkgs.writeShellScriptBin {};
            };
          in
          pkgs.mkShell {
            buildInputs = with haskellPackages; with pkgs.elmPackages; [
              haskell-language-server
              ormolu
              ghcid
              cabal-install
              scripts
              pkgs.just
              elm elm-format elm-language-server
            ];
            inputsFrom = [
              self.defaultPackage.${system}.env
            ];
          };
      });
}
