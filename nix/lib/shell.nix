{ name ## Name of the application
, pkgs ## nixpkgs
, telosnix ## Imported telos.nix
, haskell ## Our Haskell component
}:

let
  ## Get the devshell:
  devshell = telosnix.tools.devshell {
    name = "${name}-devshell";
    src = ../..;
    quickstart = ../docs/quickstart.md;
    guide = [
      { name = "quickstart"; title = "Quickstart"; path = ../docs/quickstart.md; }
    ];
    extensions = {
      build = {
        help = "Performs an unoptimized, development build";
        exec = "cabal build -O0";
      };
      test = {
        help = "Runs tests";
        exec = "cabal v1-test --ghc-options=\"-O0\"";
      };
      lint = {
        help = "Runs linters";
        exec = "hlint src/ test/";
      };
      haddock = {
        help = "Build Haskell documentation";
        exec = "cabal haddock -O0";
      };
      format = {
        help = "Formats the codebase";
        exec = "fourmolu -i src/ test/";
      };
    };
  };

  ## Declare dependencies for our shell:
  deps = [
    ## Release related:
    pkgs.gh
    pkgs.git
    pkgs.git-chglog

    ## Development shell related:
    devshell
  ];

  ## Define our shell:
  shell = pkgs.mkShell {
    buildInputs = deps ++ haskell.packageDepsDev;

    shellHook = ''
      ## Greet:
      devsh welcome

      ## Make sure that doctest finds correct GHC executable and libraries:
      export NIX_GHC=${haskell.ghc}/bin/ghc
      export NIX_GHC_LIBDIR=${haskell.ghc}/lib/${haskell.ghc.meta.name}
    '';

    DEVSHELL_ROOT = "${toString ../..}";
  };
in
{
  ## Nix shell:
  shell = shell;
}
