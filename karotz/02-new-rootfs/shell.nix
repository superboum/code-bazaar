with (import <nixpkgs> {}).pkgsi686Linux; mkShell {
  buildInputs = [ ncurses bc ];
}
