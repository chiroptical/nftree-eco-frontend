let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  tailwind-purs = pkgs.haskellPackages.callPackage ./nix/tailwind-purs.nix {};
in
  pkgs.mkShell {
    buildInputs = with pkgs; [
      purescript
      spago
      nodePackages.pscid
      nodePackages.parcel-bundler
      nodePackages.purty

      # Tailwind dependencies
      cabal2nix
      nodePackages.npm
      tailwind-purs
    ];
  }
