{ pkgs ? import ./nix/pkgs.nix {} }:

let
  tailwind-purs = pkgs.haskellPackages.callPackage ./nix/tailwind-purs.nix {};
in
  pkgs.mkShell {
    buildInputs = [
      pkgs.purescript
      pkgs.spago
      pkgs.nodePackages.pscid
      pkgs.nodePackages.parcel-bundler
      pkgs.nodePackages.purty
      pkgs.nodePackages.purescript-language-server
      tailwind-purs
    ];
  }
