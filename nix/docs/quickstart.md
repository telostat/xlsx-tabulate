# Welcome to XLSX Tabulate Development Shell

XLSX Tabulate development shell is provisioned via a Nix and it is provided for
developers to conveniently develop, test and release XLSX Tabulate.

## Useful Commands

See welcome message:

```sh
devsh-welcome
```

See the rendered version of this document:

```sh
devsh-help
```

Re-generate project `.cabal` file, format codebase, run `hlint`, build and test at once:

```sh
devsh-makedev
```

... or respectively:

```sh
hpack
fourmolu -i src/ test/
hlint src/ test/
cabal build -O0
cabal v1-test
```

Run `haddock`:

```sh
cabal haddock -O0
```
