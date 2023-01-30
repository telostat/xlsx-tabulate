{ compiler ? "ghc92"
, ...
}:

let
  nix = import ./nix { compiler = compiler; };
in
nix.components.shell.shell
