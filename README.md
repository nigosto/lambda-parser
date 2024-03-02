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
The parser traverses the lambda term and creates AST where the nodes are variables, applications and abstractions (lexing and parsing are done simultaneously). Then the substitution is done on the AST with the corresponding tokens instead of directly on the term. After the substitution, a new lambda term is generated based on the newly created AST and the result is returned as the final answer. If there are no brackets, the application term is right associative. Bear in mind that in the generated final term some brackets may be added because there is no way to know where they can be skipped without looking ahead, which the current generator does not do.

### Grammar
$` variable \Coloneqq [a-z] \\ `$

$` λ-term \Coloneqq \langle variable \rangle \\ `$

$`   \quad \quad \quad \quad \ \ | \quad (\!( \langleλ-term\rangle )\!)^* \ (\!( \langleλ-term\rangle )\!)^* \\ `$

$`   \quad \quad \quad \quad \ \ | \quad λ \ \langle variable\rangle \ . \ \langleλ-term\rangle `$

(Here $` (\!( \ )\!)^* `$ means that the brackets themselves are optional)

### Substitution
In order for the substitution to be correct and not change the semantics of the lambda terms, the algorithm uses **Curry substitution**. This means that in the cases where a naive substitution would cause a problem, the bound variables are renamed. The algorithm starts by trying all letters, starting from *a*, until a letter is found that would not cause the term to change its semantics and renames the bound variable with the found letter. If a suitable letter is not found, then the algorithm continues to search for any suitable character (which does not follow the grammar).

## License
The project is licensed under the MIT License. See the [LICENSE](./LICENSE) file for more information.

## Contacts
- GitHub: https://github.com/nigosto
- LinkedIn: https://www.linkedin.com/in/georgi-n-atanasov
- email: georgi.n.atanasov02@gmail.com