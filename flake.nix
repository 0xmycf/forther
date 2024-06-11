{
  description = "A very basic haskell flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    utils,
    ...
  }:
    utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      haskellPackages = pkgs.haskellPackages;
      overlay = final: prev: {
        forther =
          haskellPackages.callCabal2nix "forther" ./. {};
      };
      myHaskellPackages = haskellPackages.extend overlay;
    in {
      devShell = myHaskellPackages.shellFor {
        packages = p: [
          p.forther
        ];
        nativeBuildInputs = with haskellPackages; [
          ghcid
          cabal-install
          haskell-language-server
          hspec-discover
        ];
      };

      packages = rec {
        forther = myHaskellPackages.forther;
        default = forther;
      };

      app = rec {
        forther = myHaskellPackages.forther;
        default = forther;
      };
    });
}
