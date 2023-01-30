{ name ## Name of the application
, pkgs ## nixpkgs
, sources ## Niv sources
, telosnix ## Imported telos.nix
, compiler ## GHC compiler version (as recognized by nixpkgs)
}:

let
  ## Get Haskell for package development purposes:
  haskell = telosnix.tools.haskell.getHaskell {
    pkgs = pkgs;
    compiler = compiler;
    overrides = new: old: { };
  };

  ## Get this package:
  thisPackage = haskell.callCabal2nixWithOptions name ../../. "" { };

  ## Get this package's Haskell dependencies:
  thisPackageDeps = pkgs.haskell.lib.compose.getHaskellBuildInputs thisPackage;

  ## Get our GHC for development:
  thisGhc = haskell.ghcWithPackages (_: thisPackageDeps);

  ## Get Haskell development tools:
  thisHaskellDevDependencies = with haskell;
    [
      ## Our GHC with all packages required to build and test our package:
      thisGhc

      ## Various haskell tools:
      apply-refact
      cabal-install
      cabal2nix
      dotenv
      fourmolu
      haskell-language-server
      hlint
      hpack
    ];

  # ## Define a function that makes this package installable in Nix environment with all its dependencies:
  # makeThisPackageInstallable = drv: drv.overrideAttrs (oldAttrs: rec {
  #   nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [
  #     pkgs.makeWrapper
  #   ];
  #
  #   postFixup = (oldAttrs.postFixup or "") + ''
  #     wrapProgram $out/bin/${name} --prefix PATH : ${pkgs.lib.makeBinPath [ ]}
  #   '';
  # });
  #
  # ## Get the installable package (only static executable):
  # thisPackageInstallable = pkgs.haskell.lib.justStaticExecutables (makeThisPackageInstallable thisPackage);
in
{
  ## Haskell package set:
  set = haskell;
  ## Haskell compiler:
  ghc = thisGhc;
  ## Nix representation of our application as a package:
  package = thisPackage;
  ## Haskell dependencies of our application package for production:
  packageDeps = thisPackageDeps;
  ## Haskell library and tool dependencies of our application package for development:
  packageDepsDev = thisHaskellDevDependencies;
  # ## Our installable application:
  # application = thisPackageInstallable;
}
