{
  # This is the ghc that stack passes in, but we just ignore it.
  ghc,
  ...
}:
with (import ./. {});
  haskell.lib.compose.buildStackProject {
    name = "site-stack-shell";
    ghc = site-pkg-set.ghc;
    nativeBuildInputs = [
      bzip2
      zlib
      zlib.dev
    ];
  }

