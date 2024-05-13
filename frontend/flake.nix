{
  inputs = {
    nixpkgs.url     = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs: with inputs; flake-utils.lib.eachDefaultSystem (system:
  let pkgs = import nixpkgs { inherit system; };
      hask = pkgs.haskell.packages.ghc928;
  in rec {
    devShell = pkgs.mkShell {
      packages = with pkgs; [
        (hask.ghcWithPackages (ps: with ps; [
          clay
        ]))
        entr
        jq
        nodejs
      ];
    };
  });
}
