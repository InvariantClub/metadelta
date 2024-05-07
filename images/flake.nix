{
  inputs = {
    flake-utils.url     = "github:numtide/flake-utils";
    nixpkgs.url         = "github:NixOS/nixpkgs/nixos-23.11";
    unstableNixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs, unstableNixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs         = nixpkgs.legacyPackages.${system};
        unstablePkgs = unstableNixpkgs.legacyPackages.${system};
      in
      {
        devShells = {
          default = pkgs.mkShell {
            packages =
            let
              hask = pkgs.haskell.packages.ghc928.override {
                overrides = self: super: {
                  # elm-street =
                  # let src = pkgs.fetchgit {
                  #   url    = "https://github.com/Holmusk/elm-street";
                  #   rev    = "d104c24bd328144057641b684330041bbfcfc9fe";
                  #   sha256 = "sha256-SV3yR//fmAqhxN0c60DTvtyqYsFuHVLbLlqM/CQ+STY=";
                  #   };
                  # in self.callCabal2nix "elm-street" src {};
                };
              };
            in with pkgs; [
              (hask.ghcWithPackages (ps: with ps; [
                bytestring
                containers
                diagrams
                diagrams-cairo
                filepath
              ]))
            ];
          };
        };
      }
    );
}


