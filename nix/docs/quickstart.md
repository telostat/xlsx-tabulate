# Welcome to XLSX Tabulate Development Shell

XLSX Tabulate development shell is provisioned via a Nix shell and it is
provided for developers to conveniently develop, test and release XLSX Tabulate.

## Useful Commands

Perform an unoptimized, development build:

```sh
devsh exec build
```

Run tests:

```sh
devsh exec test
```

Run linters:

```sh
devsh exec lint
```

Format the codebase:

```sh
devsh exec format
```

Build Haskell documentation:

```sh
devsh exec haddock
```

Show devshell extensions:

```sh
devsh exec
```

For further help:

```sh
devsh guide
```
