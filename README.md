# Punkt0

This project is a compiler for a simple language called Punkt0.

The description of the grammar of Punkt0 is [here](./docs/punkt0.grammar)

The compiler has 6 phases:

1. [Lexer](./docs/Lexer.md) (Done)
2. [Parser](/docs/Parser.md) (Done)
3. [Analyzer](./docs/NameAnalysis.md) (In Progress)
4. [Type Checking](./docs/TypeChecking.md) (Todo)
5. [Code Generation](./docs/CodeGeneration.md) (Todo)

## Local Requirement

- JDK
- sbt

## Run

### Lexer

This run the Lexer phase of the compiler only, prints all the tokens found with their position and exits.

Many test files can be found under the directory `/test-programs/lexer/valid/` with the extension `.p0`. The files with
the extension `.check` contain the correct value that should be returned by the Lexer.

```shell
sbt run --tokens ./test-programs/lexer/valid/all.p0
```

### Parser

This run the Parser phase of the compiler only, prints the AST of the program and exits.

Many test files can be found under the directory `/test-programs/ast/valid/` with the extension `.p0`. The files with
the extension `.p0.ast` contain the correct value that should be returned by the Parser.

```shell
sbt run --ast ./test-programs/ast/valid/Simple.p0
```

### Name Analysis

Todo

### Type Checking

Todo

### Code Generation

Todo



