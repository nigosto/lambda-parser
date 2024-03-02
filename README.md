# lambda-parser
Parser for lambda terms, written in Haskell, that is also able to apply substitution,  according to the rules of lambda calculus

## Table of contents
- [Prerequisites](#prerequisites)
- [Running the project](#running-the-project)
- [Installing the executable manually](#installing-the-executable-manually)
- [Documentation](#documentation)
    - [Grammar](#grammar)
    - [Substitution](#substitution)
- [License](#license)
- [Contacts](#contacts)

## Prerequisites
- ghc 9.4.7 or newer
- cabal 3.6.2.1 or newer

## Running the project
Running the project using `cabal`:
```shell
cabal run
```

If you want to run it in REPL:
```shell
cabal repl
```

## Installing the executable manually
If you want to install the executable yourselves, then you need to run the following command:
```shell
cabal install
```

Now the executable should be located in the configured `installdir` of `cabal` (usually `~/.local/bin` or `~/.cabal/bin`).

## Documentation
TODO

### Grammar
TODO

### Substitution
TODO

## License
The project is licensed under the MIT License. See the [LICENSE](./LICENSE) file for more information.

## Contacts
- GitHub: https://github.com/nigosto
- LinkedIn: https://www.linkedin.com/in/georgi-n-atanasov
- email: georgi.n.atanasov02@gmail.com