let pkgs = import ./nix/nixpkgs.nix; in

with pkgs; {
  generator = callPackage ./nix/generator.nix {};
  solution = haskellPackages.callCabal2nix "solution" ./solution {};
}
