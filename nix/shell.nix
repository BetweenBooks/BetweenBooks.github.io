{ inputs, ... }:
with inputs; {
  perSystem =
    { lib
    , system
    , pkgs
    , hsPkgs
    , ...
  }:
    {
      devShells.default = hsPkgs.shellFor {
        packages = ps: [ ps.between-books ];
        nativeBuildInputs = with pkgs; [
          cabal-install
          imagemagick
          rsync
        ];
      };
    };
}

