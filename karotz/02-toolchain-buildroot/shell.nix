{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  hardeningDisable = [ "all" ];
  buildInputs = [
    bc
    ncurses
  ];
}

