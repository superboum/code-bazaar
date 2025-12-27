{
  description = "learnix";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem(system: 
      let
      pkgs = import nixpkgs { inherit system; };
      shell = pkgs.mkShell {
        disableHardening = [ "all" ];
        buildInputs = [
          pkgs.stdenv.cc
	  pkgs.rustc
	  pkgs.cargo
        ];
      };
      in
      {
        meta.version = "0.0.1";
	devShells.default = shell;
      }
    );
}
  
