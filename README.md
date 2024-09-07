# lambda-parser
Parser for lambda terms, written in Haskell, that is also able to execute a list of operations on the terms

## Table of contents
- [Prerequisites](#prerequisites)
- [Running the project](#running-the-project)
- [Running the tests](#running-the-tests)
- [Installing the executable manually](#installing-the-executable-manually)
- [Documentation](#documentation)
    - [Grammar](#grammar)
    - [Supported operations](#supported-operations)
        - [Substitution](#substitution)
        - [Transformation to applicative term](#transformation-to-applicative-term)
        - [Counting beta-redexes](#counting-beta-redexes)
- [License](#license)
- [Contacts](#contacts)

## Prerequisites
- ghc 9.4.7 or newer
- cabal 3.6.2.1 or newer

## Running the project
Run the project using `cabal`:
```shell
cabal run
```

If you want to run it in REPL:
```shell
cabal repl
```

## Running the tests
```shell
cabal test --test-show-details=streaming
```

## Installing the executable manually
If you want to install the executable yourselves, then you need to run the following command:
```shell
cabal install
```

Now the executable should be located in the configured `installdir` of `cabal` (usually `~/.local/bin` or `~/.cabal/bin`).

## Documentation
The parser traverses the lambda term and creates AST where the nodes are variables, applications and abstractions (lexing and parsing are done simultaneously). Then the operations are done on the AST with the corresponding tokens instead of directly on the term. After the operation, a new lambda term is generated based on the newly created AST and the result is returned as the final answer. If there are no brackets, the application term is left associative. Bear in mind that in the generated final term some brackets may be added because there is no way to know where they can be skipped without looking ahead, which the current generator does not do.

### Grammar
$` variable \Coloneqq [a-z] `$ <br>
$` λ-term \Coloneqq \langle variable \rangle `$ <br>
$`   \quad \quad \quad \quad \ \ | \quad (\!( \langleλ-term\rangle )\!)^* \ (\!( \langleλ-term\rangle )\!)^* `$ <br>
$`   \quad \quad \quad \quad \ \ | \quad λ \ \langle variable\rangle \ . \ \langleλ-term\rangle `$

(Here $` (\!( \ )\!)^* `$ means that the brackets themselves are optional)

### Supported operations
This is the list of currently supported operations, that can be executed on the terms after parsing:

#### Substitution
There are currently 2 supported versions for the substitutions:
- curry substitution - this is the standart substitution, done directly on the original term. In order for it to be correct and not change the semantics of the lambda terms, in the cases where a naive substitution would cause a problem, the bound variables are renamed. The algorithm starts by trying all letters, starting from *a*, until a letter is found that would not cause the term to change its semantics and renames the bound variable with the found letter. If a suitable letter is not found, then the algorithm continues to search for any suitable character (which does not follow the grammar);
- nameless term substitution - in order for this substitution to be applied the term is first transformed into nameless term. Nameless terms don't have variable names - instead de Bruijn indexes are used to denote which variables are bound and which - free. The substitution is then done on the indexes instead of the variables by utilizing the `shift` function. After the substitution is done, the term is transformed back into its named version, where the bound variables may have different names (which does not change the semantics of the term).

#### Transformation to applicative term 
The transformation to applicative term is non-deterministic operation because there are 2 approaches to it - either to start from the outside or the inside. The algorithm always starts from the outside, except in the case where starting from the outside would not lead to the full transformation - this is the case where there are 2 nested abstractions where the argument of the first is part of the free variables of the second. When this case is encountered the transformation is applied to the inner abstraction first and after it is done the partial result is transformed again to achieve the correct result.

#### Counting beta-redexes
Counting the beta-redexes in s single term is the simplest operation. The algorithm traverses the AST and counts the occurences of applications where the first argument is abstraction.

## License
The project is licensed under the MIT License. See the [LICENSE](./LICENSE) file for more information.

## Contacts
- GitHub: https://github.com/nigosto
- LinkedIn: https://www.linkedin.com/in/georgi-n-atanasov
- email: georgi.n.atanasov02@gmail.com