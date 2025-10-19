# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## IMPORTANT: Repository Purpose and Interaction Guidelines

**DO NOT MODIFY ANY CODE IN THIS REPOSITORY.**

This is a **self-driven learning repository** where a developer is working through "Functional Programming in Scala" (the red book). Your role is to:

1. **Review code when asked** - Provide thoughtful, high-quality reviews of specific implementations
2. **Suggest improvements** - Offer concrete suggestions that the developer can implement themselves
3. **Answer questions** - Explain functional programming concepts, design patterns, and implementation details
4. **Provide guidance** - Help the developer understand trade-offs and alternative approaches

**What NOT to do:**
- Do not write, edit, or modify any code files
- Do not create new implementations
- Do not use Write or Edit tools on source files
- Do not be proactive about "fixing" code

**What to do:**
- Wait for the developer to ask for review or guidance
- Provide explanations and teaching moments
- Suggest improvements in your responses (not through code edits)
- Help debug issues by analyzing code and suggesting what to investigate
- Run builds/tests if asked to verify behavior

## Project Overview

This is a Scala 3 learning repository containing implementations of functional programming concepts and exercises. The code appears to be working through "Functional Programming in Scala" (the red book), with each chapter implementing different functional programming patterns and data structures.

## Build System

This project uses **sbt** (Scala Build Tool) with Scala 3.7.2.

### Common Commands

- **Compile the project**: `sbt compile`
- **Run the main program**: `sbt run`
- **Run tests**: `sbt test`
- **Start Scala REPL**: `sbt console`
- **Run a specific test suite**: `sbt "testOnly MySuite"`
- **Continuous compilation**: `sbt ~compile`
- **Clean build artifacts**: `sbt clean`

### Testing Framework

The project uses **MUnit** (version 1.0.0) as its testing framework. Test files are located in `src/test/scala/`.

## Code Architecture

### Chapter-Based Organization

The codebase is organized into chapters, each implementing different functional programming concepts:

- **Chapter2.scala**: Basic functional programming fundamentals
  - Higher-order functions, currying, uncurrying, function composition
  - `factorial`, `fib`, `isSorted`, `curry`, `uncurry`, `compose`

- **Chapter3.scala**: Custom immutable data structures
  - `MyList[+A]`: Custom cons-list implementation with functional operations
  - `MyTree[+A]`: Binary tree implementation with fold-based operations
  - Both use `enum` for ADT (Algebraic Data Type) representation

- **Chapter4.scala**: Error handling without exceptions
  - `MyOption[+A]`: Custom Option type implementation
  - `MyEither[+E, +A]`: Custom Either type for error handling
  - Functions like `sequence`, `traverse`, `map2` for composing error-prone computations

- **Chapter5.scala**: Lazy evaluation and streams
  - `MyLazyList[+A]`: Custom lazy list (stream) implementation
  - Implements infinite sequences and efficient stream processing
  - Uses `unfold` as the core stream constructor

- **Chapter6.scala**: Purely functional state
  - `State[S, A]`: Opaque type for state transformations (`S => (A, S)`)
  - `RNG` trait and `SimpleRNG`: Random number generation
  - `Rand[A]`: Type alias for `State[RNG, A]`

- **Chapter7.scala**: Parallelism and concurrency
  - `Par[A]`: Library for parallel computations using `ExecutorService`
  - `Future[A]`: Non-blocking asynchronous computations with callbacks
  - Uses the `Actor` implementation from `Actor.scala` for thread-safe message passing

- **Chapter8.scala**: Property-based testing
  - `Gen[A]`: Random value generators using state
  - `SGen[A]`: Size-based generators (`Int => Gen[A]`)
  - `Prop`: Properties that can be tested and composed
  - `Result`: Test results (Passed, Falsified, Proved)

- **Chapter9.scala**: Parser combinators
  - `Parsers[ParseError, Parser[+_]]`: Trait defining parser combinator API
  - JSON parser implementation as example
  - Combinator functions: `or`, `product`, `many`, `flatMap`, `map2`

### Actor.scala

Contains a lock-free, non-blocking `Actor[A]` implementation copied from the fpinscala repository. This is used in Chapter 7's parallelism library for safe concurrent message passing. The implementation is based on a non-intrusive MPSC (Multi-Producer Single-Consumer) queue.

**Important**: This file is licensed under the MIT License from Manning Publications and scalaz contributors. Do not modify unless necessary.

### Main.scala

The main entry point currently demonstrates property-based testing from Chapter 8, testing list sorting and max properties.

## Key Design Patterns

### Opaque Types

The codebase extensively uses Scala 3's **opaque types** for zero-cost abstractions:
- `State[S, A]` - wraps `S => (A, S)`
- `Par[A]` - wraps `ExecutorService => Future[A]`
- `Future[A]` - wraps `(A => Unit) => Unit`
- `Gen[A]`, `SGen[A]`, `Prop`, `MaxSize`, `TestCases`

When working with opaque types, remember:
- Access underlying implementation only within the companion object
- Use extension methods for public API
- Constructors are typically in companion objects (`apply`, `unit`)

### ADTs with Enums

All algebraic data types use Scala 3's `enum` syntax:
```scala
enum MyList[+A]:
  case Nil
  case Cons(head: A, tail: MyList[A])
```

### Functional Core Patterns

- **fold-based implementations**: Most operations are implemented using `foldRight` or `foldLeft`
- **No mutable state**: All data structures are immutable
- **Tail recursion**: Most recursive functions use `@annotation.tailrec` for stack safety
- **Laziness**: Heavy use of by-name parameters (`=> A`) for deferred evaluation

## Code Style

### Scala 3 Features Used

- Significant indentation (braceless syntax)
- `enum` for ADTs instead of sealed traits
- Opaque types instead of value classes
- Extension methods instead of implicit classes
- Top-level definitions (no need for objects)
- `infix` keyword for custom operators
- New control structure syntax: `if-then-else` without parentheses

### Naming Conventions

- Type parameters: Single uppercase letters (`A`, `B`, `C`, `S`)
- Custom types prefix with `My` to distinguish from stdlib: `MyList`, `MyOption`, `MyEither`, `MyLazyList`
- Combinator operators follow Haskell conventions: `**` (product), `|` (or), `*>` (pickRight), `<*` (pickLeft)

## Development Workflow

1. Each chapter builds on previous concepts - understand earlier chapters before modifying later ones
2. The REPL (`sbt console`) is useful for experimenting with implementations
3. When adding new functionality to a chapter, maintain the functional purity and immutability principles
4. Most implementations avoid pattern matching in favor of fold/recursion where possible

## Dependencies

- **Scala**: 3.7.2
- **MUnit**: 1.0.0 (test only)
- No other runtime dependencies - this is a pedagogical project focusing on implementing concepts from scratch
