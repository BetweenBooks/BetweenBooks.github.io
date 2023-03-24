final: prev: {
  site-stacklock = final.stacklock2nix {
    stackYaml = ../stack.yaml;
  };

  site-pkg-set = final.haskell.packages.ghc925.override (oldAttrs: {
    overrides = final.lib.composeManyExtensions [
      (oldAttrs.overrides or (_: _: {}))

      final.site-stacklock.stackYamlResolverOverlay
      final.site-stacklock.stackYamlExtraDepsOverlay
      final.site-stacklock.stackYamlLocalPkgsOverlay
      final.site-stacklock.suggestedOverlay

      (hfinal: hprev: rec {
        # Missing dep.
        digest        = final.haskell.lib.addBuildDepend hprev.digest final.pkgs.zlib;
        # Some tests don't work
        unliftio-core = final.haskell.lib.dontCheck hprev.unliftio-core;
        pandoc        = final.haskell.lib.dontCheck hprev.pandoc;
        hspec-contrib = final.haskell.lib.dontCheck hprev.hspec-contrib;
      })
    ];

    all-cabal-hashes = final.fetchurl {
      name = "all-cabal-hashes";
      url =
        let commit = "4d0865132ec79c472a3e10d385324e7e96467eab";
        in "https://github.com/commercialhaskell/all-cabal-hashes/archive/${commit}.tar.gz";
      sha256 = "sha256-SB/GkRzvQrv3d2Yrkjb+mMDXNLU69/8mxfRp6ruEpgQ=";
    };
  });

  site-app = final.site-pkg-set.between-books-site;

  # Actually, the dev-shell for the entire project.
  site-dev-shell = final.site-pkg-set.shellFor rec {
    packages = haskPkgs: final.site-stacklock.localPkgsSelector haskPkgs;
    nativeBuildInputs = [
      final.cabal-install
      final.stack
      # Random things that could/should be cleaned up.
      final.rsync
      final.imagemagick
    ];
  };
}

