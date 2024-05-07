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

        yaml-includes = pkgs.haskellPackages.developPackage
        {
          root = ./yaml-includes;
        };

        hasura-to-icf = pkgs.haskellPackages.developPackage
        {
          root = ./hasura-to-icf;
        };

        metadelta-cli = pkgs.haskellPackages.developPackage
        {
          root = ./cli;
          # Note: This is a bit of a hack; we just provide these manually.
          # This is because `developPackage` doesn't know about cabal.project
          # files.
          overrides = self: super: {
            yaml-includes = yaml-includes;
            hasura-to-icf = hasura-to-icf;
          };
        };

        metadelta-cli-img =
          pkgs.dockerTools.buildLayeredImage
          {
            name = "metadelta-cli";
            tag  = "latest";
            config = {
              Entrypoint = "${pkgs.haskell.lib.justStaticExecutables metadelta-cli}/bin/metadelta";
              Labels = {
                "org.opencontainers.image.source"      = "https://github.com/InvariantClub/metadelta";
                "org.opencontainers.image.description" = "metadelta";
              };
            };
          };
      in
      {
        packages = {
          metadelta-docker-image = metadelta-cli-img;
        };

        devShells = {
          default = pkgs.mkShell {
            packages =
            let
              watchWithGhcid = pkgs.writers.writeDashBin "watch" ''
                ${pkgs.ghcid}/bin/ghcid --command="cabal repl"
              '';
              # Wrap cabal to always run `hpack` first.
              cabalWrapped = pkgs.writers.writeDashBin "cabal" ''
                ${pkgs.hpack}/bin/hpack >/dev/null 2>&1
                ${pkgs.cabal-install}/bin/cabal "$@"
              '';
              hask = pkgs.haskell.packages.ghc96.override {
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
              cabalWrapped
              hpack
              ghcid
              stylish-haskell
              watchWithGhcid

              (hask.ghcWithPackages (ps: with ps; [
                HUnit
                QuickCheck
                aeson
                bytestring
                containers
                directory
                dotenv
                extra
                filepath
                generic-lens
                hspec
                lens
                mtl
                optparse-applicative
                optparse-generic
                patience
                random
                template-haskell
                text
                time
                vector
                yaml

                # repo-change-comparer
                turtle
              ]))
            ];
          };
        };
      }
    );
}


