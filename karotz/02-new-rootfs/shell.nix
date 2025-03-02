{ pkgs ? import <nixpkgs> {} }:

(pkgs.buildFHSUserEnv {
  name = "buildroot";
  targetPkgs = pkgs: (with pkgs; [
    ncurses
    busybox
    bc
    gnumake
    file
    stdenv.cc.cc.lib
  ]);
}).env
