{ pkgs ? import <nixpkgs> {}
}:

pkgs.mkShell {
  buildInputs = with import ./default.nix { inherit pkgs; }; [
    generator
    solution
  ];
}
