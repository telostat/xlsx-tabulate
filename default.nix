{ sources ? import ./nix/sources.nix
, compiler ? "default"
, system ? builtins.currentSystem
, ...
}:

let
  ##################
  ## LOAD NIXPKGS ##
  ##################

  ## Import nixpkgs pinned by niv:
  pkgs = import sources.nixpkgs { inherit system; };

  ##################
  ## LOAD HELPERS ##
  ##################

  ## Load the YAML reader:
  readYAML = pkgs.callPackage ./nix/lib/read-yaml.nix { };

  ## Load Haskell package factory:
  mkHaskell = pkgs.callPackage ./nix/lib/mk-haskell.nix { };

  #############
  ## HASKELL ##
  #############

  ## Get Haskell packages in the project:
  thisHaskellPackages = {
    main = {
      name = "xlsx-tabulate";
      path = ./.;
    };
    subs = [ ];
  };

  ## Get Haskell packages in the project as a list:
  thisHaskellPackagesAll = [ thisHaskellPackages.main ] ++ thisHaskellPackages.subs;

  ## Get the main Haskell package specification:
  packageSpec = readYAML (thisHaskellPackages.main.path + "/package.yaml");

  ## Get base Haskell package set:
  baseHaskell = if compiler == "default" then pkgs.haskellPackages else pkgs.haskell.packages.${compiler};

  ## Get this Haskell package set:
  thisHaskell = mkHaskell {
    haskell = baseHaskell;
    packages = thisHaskellPackagesAll;
    overrides = self: super: {
      xlsx = super.xlsx_1_1_0_1;
    };
  };

  ###########
  ## SHELL ##
  ###########

  ## Prepare Nix shell:
  thisShell = thisHaskell.shellFor {
    ## Define packages for the shell:
    packages = p: builtins.map (x: p.${x.name}) thisHaskellPackagesAll;

    ## Enable Hoogle:
    withHoogle = false;

    ## Build inputs for development shell:
    buildInputs = [
      ## Haskell related build inputs:
      thisHaskell.apply-refact
      thisHaskell.cabal-fmt
      thisHaskell.cabal-install
      thisHaskell.cabal2nix
      thisHaskell.fourmolu
      thisHaskell.haskell-language-server
      thisHaskell.hlint
      thisHaskell.hpack

      ## Other build inputs for various development requirements:
      pkgs.git
    ];

    ## Shell hook for development shell:
    shellHook = ''
      ## Environment variables:
      export PROJECT_DEV_ROOT="$(git rev-parse --show-toplevel)"

      ## Shell aliases:
      alias riched="${pkgs.rich-cli}/bin/rich --emoji --center --width 120 --panel rounded --theme rrt --hyperlinks"
      alias devsh-help="riched --pager ''${PROJECT_DEV_ROOT}/nix/docs/quickstart.md"
      alias devsh-welcome="riched ''${PROJECT_DEV_ROOT}/README.md"
      alias devsh-makedev="hpack && fourmolu -i src/ test/ && hlint src/ test/ && cabal build -O0 && cabal v1-test"

      ## Greet:
      devsh-welcome
      echo
      echo '**Run devsh-welcome to see Welcome notice again**' | riched -m -
      echo '**Run devsh-help to see help**' | riched -m -
    '';
  };
in
thisShell
