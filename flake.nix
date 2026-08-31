{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    import-tree.url = "github:vic/import-tree";
    flake-parts.url = "github:hercules-ci/flake-parts";
    treefmt-nix.url = "github:numtide/treefmt-nix";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; }
      {
        systems = [
          "aarch64-darwin"
          "aarch64-linux"
          "x86_64-darwin"
          "x86_64-linux"
        ];

        imports = [
          (inputs.import-tree ./nix)
        ];
      };
}
