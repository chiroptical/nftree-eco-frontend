let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  easy-ps = import sources.easy-purescript-nix { inherit pkgs; };
  tailwind-purs = pkgs.haskellPackages.callPackage ./nix/tailwind-purs.nix {};
in
  pkgs.mkShell {
    buildInputs = with pkgs; [
      purescript
      spago
      nodePackages.parcel-bundler
      easy-ps.purescript-language-server
      easy-ps.purs-tidy
      easy-ps.pscid

      # Tailwind dependencies
      tailwind-purs

      # Dev dependencies
      niv
    ];
  }
