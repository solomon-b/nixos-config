# Code Review

Perform a thorough code review of recent changes with focus on bugs, security, best practices, functional programming principles, and testing.

**Usage patterns:**
- `/ssb-review` - Review commits ahead of master/main (default)
- `/ssb-review wip` - Review staged and unstaged changes
- `/ssb-review 15` - Review last 15 commits
- `/ssb-review develop` - Review commits ahead of develop branch

**Instructions:**

1. **Determine scope:**
   - If user message contains "wip", "staged", "unstaged", or "changes", review working directory changes
   - If a number is provided, review that many commits
   - If a branch name is provided, review commits ahead of that branch
   - Otherwise (default), review commits ahead of main/master branch

2. **Gather changes to review:**
   
   **For work-in-progress mode:**
   - Run `git status`
   - Run `git diff` for unstaged changes
   - Run `git diff --cached` for staged changes
   - Capture the full diff output for analysis
   
   **For commit history mode:**
   - Determine commit range:
     - If number: `git log -n <number>`
     - If branch: `git log origin/<branch>..HEAD`
     - If default: Check for master/main, use `git log origin/master..HEAD`
   - For each commit: `git show <commit-hash>`
   - Compile all changes for review

3. **Perform comprehensive review focusing on:**

   **Critical Issues (High Priority):**
   - Security vulnerabilities (SQL injection, XSS, authentication issues, exposed secrets)
   - Logic errors and bugs (off-by-one, null/undefined handling, race conditions)
   - Memory leaks or resource management issues
   - Error handling gaps (uncaught exceptions, missing error cases)
   - Data integrity issues (validation, type safety violations)
   - Partial functions that could crash (head, tail, !!, fromJust)
   - Incomplete pattern matches

   **Testing (High Priority):**
   
   *Test Coverage and Quality:*
   - Are there tests for the new/modified code?
   - Do tests cover the happy path AND edge cases?
   - Are error cases tested?
   - Do tests verify actual behavior, not just implementation details?
   - Are tests clear and understandable?
   
   *Property-Based Tests (QuickCheck/Hedgehog):*
   - For pure functions, are there property tests?
   - Do properties express invariants and laws?
   - Examples of good properties:
     - Round-trip properties (parse . render == id)
     - Algebraic laws (associativity, identity, commutativity, etc.)
     - Invariants (sorted list stays sorted after insert)
     - Oracle properties (comparing to reference implementation)
   - Are generators appropriate and comprehensive?
   - Are shrinking strategies working well?
   
   *Unit Tests:*
   - Do unit tests cover specific cases and examples?
   - Are edge cases explicitly tested (empty, single element, boundary values)?
   - Are error conditions tested?
   - Do tests use meaningful names that describe what they test?
   - Are tests isolated and independent?
   
   *Test Quality Issues:*
   - Brittle tests that break on refactoring
   - Tests that are hard to understand
   - Missing negative tests (things that should fail)
   - Incomplete coverage of code paths
   - Tests that test the wrong thing
   - Flaky or non-deterministic tests

   **Functional Programming Principles (High Priority):**
   
   *Simplicity, Elegance, and Clarity (PARAMOUNT):*
   - Is the code immediately understandable?
   - Does the solution feel "obvious" in hindsight?
   - Are there simpler ways to express the same idea?
   - Is complexity justified or accidental?
   - Would a beginner understand the intent?
   - Is the code self-documenting through clear names and types?
   
   *Parse, Don't Validate:*
   - Are domain invariants enforced at parse time with types?
   - Are validated values wrapped in newtypes to prevent misuse?
   - Is validation logic duplicated across the codebase?
   - Are "stringly-typed" or primitive-obsessed APIs used instead of proper types?
   - Are there property tests for parsers?
   
   *Compositionality:*
   - Can functions be easily combined and reused?
   - Are there large monolithic functions that should be decomposed?
   - Do functions have clear, single responsibilities?
   - Are there opportunities to extract reusable combinators?
   - Is the composition obvious and natural?
   
   *Purity and Referential Transparency:*
   - Are side effects properly isolated and tracked in types (IO, State, etc.)?
   - Are there hidden side effects that should be explicit?
   - Can pure functions be extracted from impure ones?
   - Is there unnecessary use of IO when pure code would work?
   - Are pure functions tested with property tests?
   
   *Function Decomposition:*
   - Are functions too large (>30 lines is a red flag)?
   - Can complex functions be split into named helper functions?
   - Are there deeply nested conditionals that could be flattened?
   - Would a where clause or let binding improve clarity?
   - Does each function do one clear thing?
   - Can each piece be tested independently?
   
   *Denotational Design:*
   - Do types accurately model the domain?
   - Are illegal states unrepresentable in the type system?
   - Do function signatures clearly express intent and constraints?
   - Are there "magic values" or sentinel values that should be types?
   - Is the semantic correspondence clear and direct?
   - **Have algebraic laws been identified for the types/operations?**
   - **Are invariants explicitly documented and enforced?**
   - **Can the design be understood through its algebra?**
   - Examples of laws to look for:
     - Associativity: `(a <> b) <> c == a <> (b <> c)`
     - Identity: `mempty <> a == a` and `a <> mempty == a`
     - Commutativity: `a <> b == b <> a` (when applicable)
     - Distributivity: `f (a <> b) == f a <> f b` (homomorphisms)
     - Absorption: operations that cancel each other
   - Are these laws tested with property tests?
   
   *Other FP Principles:*
   - Totality: Are all functions total, or are partial cases handled?
   - Algebraic thinking: Do types form proper algebras with lawful operations?
   - Abstraction: Are the right abstractions used (Functor, Applicative, Monad, etc.)?
   - Immutability: Is data needlessly mutated when immutable approaches exist?

   **Code Quality (Medium Priority):**
   - Code duplication or missed abstraction opportunities
   - Overly complex functions that should be refactored
   - Inconsistent naming conventions or style violations
   - Missing or inadequate documentation
   - Hard-coded values that should be configurable
   - Performance concerns (N+1 queries, inefficient algorithms, space leaks)
   - Lazy evaluation issues (thunk buildup, strictness annotations needed)
   - Over-engineering or premature optimization

   **Best Practices (Lower Priority):**
   - Edge case handling
   - Type safety improvements (newtypes, phantom types, GADTs where appropriate)
   - Opportunities for more precise types (NonEmpty, Refined, etc.)
   - Documentation and examples
   - Code organization and module structure

   **Positive Observations:**
   - Simple, elegant solutions that feel "right"
   - Code that reads like prose
   - Excellent test coverage with both property and unit tests
   - Well-designed types that make illegal states unrepresentable
   - Clear algebraic laws and invariants
   - Elegant use of composition and higher-order functions
   - Good separation of pure and impure code
   - Clear, self-documenting function signatures
   - Effective use of type classes and abstractions
   - Property tests that express laws and invariants
   - Tests that serve as documentation
   - Appropriate use of abstraction (not too much, not too little)

4. **Structure your review output:**
```
   🔍 Code Review Summary
   
   Scope: [description of what was reviewed]
   Files Changed: [count and key files]
   
   ⚠️ Critical Issues:
   - [Issue 1 with file:line reference]
   - [Issue 2 with file:line reference]
   
   🧪 Testing Concerns:
   - Missing Tests: [what needs testing]
   - Test Coverage: [gaps in coverage]
   - Property Tests: [missing properties or poor generators]
   - Unit Tests: [missing edge cases or error cases]
   - Test Quality: [brittle, unclear, or wrong tests]
   - Missing Law Tests: [algebraic laws that should be tested]
   
   🎯 Functional Programming Concerns:
   - Simplicity & Clarity: [is the code clear and obvious?]
   - Parse, Don't Validate: [issues with validation/parsing]
   - Compositionality: [issues with function composition]
   - Purity: [issues with side effects or referential transparency]
   - Function Size: [functions that should be decomposed]
   - Type Design: [issues with domain modeling]
   - Algebraic Laws: [missing or unclear laws and invariants]
   
   ⚡ Code Quality Concerns:
   - [Concern 1]
   - [Concern 2]
   
   💡 Suggestions:
   - [Concrete improvement 1 - prefer simpler approaches]
   - [Concrete improvement 2 - emphasize clarity]
   - [Test suggestion 1 - what properties to test]
   - [Test suggestion 2 - what cases to cover]
   - [Law suggestion - what algebraic laws apply]
   
   ✅ Positive Notes:
   - [Good thing 1]
   - [Good thing 2]
   
   Overall Assessment: [APPROVE / APPROVE WITH COMMENTS / NEEDS WORK]
```

5. **Be specific and constructive:**
   - Always reference file names and line numbers when possible
   - Explain *why* something violates FP principles, not just *what* is wrong
   - Suggest concrete improvements with example code when helpful
   - Show how to refactor toward better composition and types
   - Emphasize simpler, more elegant solutions
   - Call out unnecessary complexity
   - Suggest specific properties to test for pure functions
   - Identify algebraic laws that should hold and be tested
   - Identify edge cases that should have unit tests
   - Balance criticism with recognition of good functional design
   - Prioritize issues by severity and impact on correctness and clarity

6. **For Haskell-specific reviews, also check:**
   - Use of partial functions (head, tail, !!, fromJust, read)
   - Incomplete pattern matches (enable -Wall -Werror in review)
   - String vs Text/ByteString usage
   - List vs Vector for performance-critical code
   - Lazy vs strict evaluation trade-offs
   - Opportunities for better types:
     - Newtypes for domain concepts
     - Phantom types for state machines
     - GADTs for typed ASTs
     - Type families for type-level programming
   - Lawful typeclass instances (Functor, Applicative, Monad laws)
   - Smart constructors that enforce invariants
   - Proper use of Maybe/Either vs exceptions
   - Over-use of type-level programming when simple types suffice
   - Excessive abstraction that obscures intent
   - Test suite organization (HSpec, Tasty, etc.)
   - Property test generators (Arbitrary instances)

**Testing Best Practices to Look For:**

*Property Tests (QuickCheck/Hedgehog):*
- Pure functions should have property tests
- Properties should express:
  - Laws (Functor laws, Monad laws, Semigroup laws, etc.)
  - Invariants (data structure properties)
  - Round-trip properties
  - Equivalences with simpler implementations
- Generators should cover edge cases
- Use of `==>` for preconditions
- Shrinking should help find minimal failing cases

*Algebraic Law Testing Examples:*
```haskell
-- Semigroup associativity
prop_assoc :: Config -> Config -> Config -> Property
prop_assoc a b c = (a <> b) <> c === a <> (b <> c)

-- Monoid identity
prop_leftIdentity :: Config -> Property
prop_leftIdentity a = mempty <> a === a

prop_rightIdentity :: Config -> Property
prop_rightIdentity a = a <> mempty === a

-- Functor laws
prop_functorIdentity :: ConfigTree a -> Property
prop_functorIdentity t = fmap id t === t

prop_functorComposition :: Fun b c -> Fun a b -> ConfigTree a -> Property
prop_functorComposition (Fn f) (Fn g) t = 
  fmap (f . g) t === (fmap f . fmap g) t

-- Custom domain laws
prop_mergeCommutative :: Config -> Config -> Property
prop_mergeCommutative a b = merge a b === merge b a

-- Invariant preservation
prop_balancedAfterInsert :: Tree a -> a -> Property
prop_balancedAfterInsert t x = 
  isBalanced t ==> isBalanced (insert x t)
```

*Unit Tests:*
- Happy path coverage
- Edge cases: empty, single element, boundary values
- Error conditions and failure modes
- Integration points
- Business logic correctness

*Good Test Examples to Suggest:*
```haskell
-- Property test for parser
prop_parseRender :: Value -> Property
prop_parseRender v = parseValue (renderValue v) === Right v

-- Property test for invariant
prop_sortedStaysSorted :: [Int] -> Int -> Property
prop_sortedStaysSorted xs x = 
  let sorted = sort xs
  in isSorted sorted ==> isSorted (insert x sorted)

-- Unit test for edge case
it "handles empty input" $ do
  parseConfig "" `shouldBe` Left EmptyConfigError

-- Unit test for specific behavior
it "validates email format" $ do
  parseEmail "valid@example.com" `shouldSatisfy` isRight
  parseEmail "invalid" `shouldSatisfy` isLeft
```

**Review Philosophy:**
- Simple is better than complex, complex is better than complicated
- Clarity and obviousness are features, not bugs
- The best code is code that doesn't need to be clever
- Elegance comes from finding the right abstraction, not the fanciest one
- If it's hard to understand, it's probably too complex
- **Property tests are your friend - use them for pure functions**
- **Good tests are documentation that never lies**
- **Identify the algebra - what are the laws and invariants?**
- **Test the laws, not just examples**
- Correctness through types and composition over runtime checks
- Make illegal states unrepresentable
- Prefer parsing with types over validation with predicates
- Small, composable, pure functions over large, monolithic, impure ones
- Focus on bugs and security issues first, then testing, then simplicity and clarity, then FP principles, then style
- Be concise - don't nitpick minor style issues unless asked
- Prioritize maintainability, correctness, clarity, compositionality, and testability
- Assume good intent, provide constructive feedback
- Question complexity - is this really necessary?
- Favor boring, obvious solutions over clever ones
- Always ask: "How would I test this?" - if it's hard to test, it needs refactoring
- Always ask: "What are the laws?" - if you can't identify them, the design may be unclear
- If everything looks good, say so clearly and call out excellent simple design and test coverage
