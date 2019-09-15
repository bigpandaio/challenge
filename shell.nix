let pkgs = import ./nix/nixpkgs.nix; in

pkgs.mkShell {
  buildInputs = with import ./default.nix; [
    generator
    solution
  ];
}
