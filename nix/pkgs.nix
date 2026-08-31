{ inputs, self, ... }: {
  perSystem = { config, system, compiler, lib, ... }:
    let
      # The single GHC the site is built against (ghc912).
      ghcVersion = "ghc9124";

      # Source filter: stop cache/build dirs invalidating the store path.
      src = lib.cleanSourceWith {
        src = self + "/";
        name = "between-books";
      };

      hsLib = prev: prev.haskell.lib.compose;

      overlay = final: prev:
        let
          hl = hsLib prev;
        in
        {
          haskell = prev.haskell // {
            packages = prev.haskell.packages // {
              ${ghcVersion} = prev.haskell.packages.${ghcVersion}.extend
                (hfinal: hprev: {
                  between-books =
                    hfinal.callCabal2nix "between-books" src { };
                  unicode-data = hl.dontCheck hprev.unicode-data;
                  sandwich = hl.dontCheck hprev.sandwich;
                });
            };
          };
        };

      pkgs = import inputs.nixpkgs {
        inherit system;
        # hakyll-diagrams is flagged broken in nixpkgs but is required by
        # the site (Hakyll.Web.Diagrams), so permit it through.
        # config.allowBroken = true;
        overlays = [
          overlay
        ];
      };
    in
    {
      _module.args = {
        inherit pkgs;
        hsPkgs = pkgs.haskell.packages.${ghcVersion};
      };
    };
}


