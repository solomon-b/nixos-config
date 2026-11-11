---
name: solomon-style
description: Use when writing, designing, refactoring, or reviewing code in any language, or writing commit messages, for Solomon. Gives his design method, his use of category theory abstractions, program structure, testing, git conventions, and the Haskell and Nix forms of these rules.
---

# Solomon's programming conventions

These rules come from 13 repos and the blog. Part 1 applies in every language. Apply it in the form that suits the language. Parts 2 and 3 give the Haskell and Nix forms.

# Part 1. General principles

## 1. Design method

1. Write an informal spec of the domain. Then find the smallest type that captures it.
2. Look for the lawful structure in that type. Ask how each parameter varies (input, output, or both), and which standard abstractions follow from that.
3. Before you write a new operation, look for an existing abstraction with that shape. Use it and say which one it is.
4. Treat a type that fits many lawful structures as a good type. Treat an encoding that breaks composition as a bad one.
5. Build narrow components. Each component takes only the input and state that it needs. Combine components with general combinators. Choose on purpose how each part combines: as a pair, as a choice, or as both.
6. Keep the description of a program separate from its interpretation. Put protocol adapters (a parser and printer pair) at the edge.
7. Put invariants in types. Tie each proof to the value that it is about. Make impossible cases unrepresentable, so callers have no need to handle them.
8. Hide advanced type machinery behind a clean interface. Show the simple code that the end user writes.
9. Prefer the least powerful tool that permits static analysis. Example: a parser generator over parser combinators for a real grammar.
10. Put recursion in folds or eliminators. Keep step functions non-recursive.
11. Start with a naive version. Name its defect. Fix one defect per step.
12. Let the type checker find the next step. Leave a hole, read what it needs, then fill it.
13. For a library, design the interface first. Write the code that a user will write, then implement it.
14. Say openly which parts are provisional and which choices are still open.

## 2. Abstractions

Use an abstraction from category theory or algebra when the problem has that structure. No rule decides this. It is a judgment. For worked examples, see these blog posts in solomon-b/blog.cofree.coffee:

- "How To Design A Chat Bot" (2022-02-14). A bot type becomes Profunctor, Strong, Choice, and Category, and each instance solves a real problem.
- "Lensy Moore" (2024-07-02). Moore machines are lenses, and the tensor is a monoidal functor.
- "Chat Bots Revisited" (2025-03-05). Narrow bots combine with tensors.

When you use one:

- Document it with its categorical definition and its laws. Link nLab for terms.
- Test its laws.
- Give each combinator a clear word name. An operator alias is optional.
- Abstract over the category when the domain has arrows other than plain functions. Examples are opposite arrows, isomorphisms, and effectful arrows.
- Get a dual structure from its dual. Example: get a cartesian structure on the opposite category from a cocartesian structure on the base category.
- Make the structure (the category, the tensors, and the units) explicit in the interface.

## 3. Program structure

- Wrap domain primitives in their own types.
- Keep the application context in one record or object.
- Model each effect as a small interface with one or two operations.
- Model each kind of error as its own type. Log an error at the point where you raise it.
- Use typed errors for failures that the caller must explain.
- Acquire resources once at the top of the program, and release them by scope.
- Prefer raw SQL to a typed query DSL.
- Let the domain shape the module layout. Examples are one module per endpoint, or one module per table.
- Give library modules an explicit public interface.
- When a component stabilizes, factor it out as its own library.

## 4. Code habits

- Write a type signature or annotation on every top-level definition.
- Use the formatter of the language at its default settings.
- Separate the sections of a file with a clear divider line.
- Keep comments short and factual. Explain why, not what.
- Mark known gaps with `TODO:`. Put a URL in a comment to point to a source.

## 5. Testing

- Use property tests for general behavior and example tests for specific cases.
- Write law tests for each lawful abstraction.
- Test persistence with round-trip properties against a real database.
- For interpreters and bots, write tests as example scripts or golden files.

## 6. Language implementations

- Write bidirectional type checkers. Give each typing rule its own function, and keep `check` and `synth` as dispatch only.
- Document each rule function with the inference rule that it implements.
- Use normalization by evaluation for conversion checks.
- In production code, attach source spans to tokens, AST nodes, and errors.
- For a real grammar, use a parser generator.

## 7. Git

- Write a one-line commit subject with no body.
- Make each commit one small change.
- Write the subject as a type prefix, then a capitalized verb in the third person, then a period. Example: `feat: Adds MonadMask instance.`
- Use a bare type prefix, such as `feat:`, `fix:`, `chore:`, or `refactor:`.

# Part 2. Haskell

## Modules and imports

- Use an 80-character `----...` line as the section divider. An optional `-- Heading` line can follow it.
- Use `ImportQualifiedPost`: `import Data.Text qualified as Text`.
- Import a type unqualified on its own line next to the qualified import: `import Data.Text (Text)`.
- Use full-word aliases (`Text`, `Aeson`).
- Give explicit lists on unqualified imports.
- Put orphan instances in their own modules.

## Language and options

- Use GHC2021 for new projects. In an existing project, keep its language setting.
- Put the shared extensions in a cabal `common` stanza under `default-extensions`. Use per-file pragmas for rare extensions.
- Use this warning set: `-Wall -Wcpp-undef -Widentities -Wincomplete-record-updates -Wincomplete-uni-patterns -Wpartial-fields -Werror=missing-home-modules`.
- Format with ormolu.

## Style

- Write a signature on every instance method (`InstanceSigs`).
- Use `where` for local helpers.
- Use `\case` for dispatch.
- Use `RecordWildCards` and `NamedFieldPuns` freely.
- Use `pure`.
- Write combinators point-free when they only re-plumb a method. Write instance bodies with explicit pattern matches.
- Write a deriving strategy on each deriving clause.
- Use newtypes for domain primitives. Name accessors with `run`, `get`, or `un`.
- Use short, meaningful type variables: `m s i o`, `s t a b`.

## Libraries

- Group export lists with `-- * Section` headers, one item per line.
- Name deriving-via adapters `From<Class>` (`FromApplicative`, `FromFunctor`).
- Give each operator a word-named function, and define the operator as an alias with its `infixr` line above it.
- Use `MINIMAL` pragmas and default methods for derived operations.
- Write a standalone kind signature when the kind is not obvious.
- Give methods `>>>` doctest examples.
- Use visible type applications in examples and tests to select instances.

## Categorical abstractions

These are the Haskell forms of the rules in Part 1, section 2. monoidal-functors and kindly-functors are the reference examples.

- Write classes over a `cat` parameter. Give instances at `(->)`, `Op`, `Iso cat`, `Star m`, and `Kleisli m`.
- Order class parameters as category, then tensors and units, then the functor last: `Monoidal cat t1 i1 t0 i0 f`.
- Name generalized operations with a `g` prefix (`gbimap`, `greturn`, `gbind`).
- In Haddock, write the definition in LaTeX, a `=== Laws` section, and the laws in an `@ ... @` block with `≡`.

## Applications

- Define the app monad as a newtype over a function from a context to `IO`. Derive its instances `via ReaderT`.
- Model effects as small capability type classes.

## Testing

- Use hedgehog for properties and hspec for examples.

# Part 3. Nix

- Give each project a flake with pinned inputs and a dev shell.
- Format Nix with nixpkgs-fmt.
- In a repo with a CLAUDE.md, follow its layout and conventions.
- Track a stable channel. When you take a package from unstable, add a comment that gives the reason and the condition for its removal.
