{
  inputs = {
      nixpkgs.url       = "github:nixos/nixpkgs/nixos-22.11";
      stacklock2nix.url = "github:cdepillabout/stacklock2nix/main";
  };

  outputs = inputs:
    with inputs; let
      supportedSystems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system:
        import nixpkgs {
          inherit system;
          overlays = [stacklock2nix.overlay self.overlay];
        });
    in {
      overlay = import nix/overlay.nix;

      packages = forAllSystems (system: {
        site = nixpkgsFor.${system}.site-app;
      });

      defaultPackage = forAllSystems (system: self.packages.${system}.site);

      devShell = forAllSystems (system: nixpkgsFor.${system}.site-dev-shell);
    };
}
