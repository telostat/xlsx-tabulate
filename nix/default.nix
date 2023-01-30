{ compiler ? "ghc92" ## GHC compiler version (as recognized by nixpkgs).
, ...
}:

let
  ## Set the name:
  name = "xlsx-tabulate";

  ## Import sources:
  sources = import ./sources.nix;

  ## Import telosnix:
  telosnix = import sources.telosnix { };

  ## Import nixpkgs (we are using `nixpkgs-unstable`):
  pkgs = import telosnix.pkgs-sources.unstable { };

  ## Get the haskell component:
  componentHaskell = import ./lib/haskell.nix {
    name = name;
    pkgs = pkgs;
    sources = sources;
    telosnix = telosnix;
    compiler = compiler;
  };

  ## Get the shell component:
  componentShell = import ./lib/shell.nix {
    name = name;
    pkgs = pkgs;
    telosnix = telosnix;
    haskell = componentHaskell;
  };
in
{
  name = name;
  pkgs = pkgs;
  sources = sources;
  telosnix = telosnix;
  components = {
    haskell = componentHaskell;
    shell = componentShell;
  };
}
