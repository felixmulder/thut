with import (builtins.fetchGit {
  name = "nixos-release-19.03";
  url = https://github.com/nixos/nixpkgs/;
  ref = "release-19.03";
}) {};

stdenv.mkDerivation {
  name = "thut";
  buildInputs = with haskell.packages.ghc864; [
    cabal-install
    hdevtools
    hoogle
    hlint
    ghcid
  ];
}
