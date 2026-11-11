# Fix Review Issues

Act on code review findings by implementing suggested improvements, with emphasis on functional programming principles, simplicity, and testing.

**Usage patterns:**
- `/ssb-fix-review` - Fix issues from the most recent review in this session
- `/ssb-fix-review critical` - Only fix critical/high-priority issues
- `/ssb-fix-review fp` - Focus on functional programming improvements
- `/ssb-fix-review file.hs` - Fix issues in a specific file
- `/ssb-fix-review "commit message here"` - Fix issues and commit with custom message
- `/ssb-fix-review critical "fix security issues"` - Combine scope and commit message

**Instructions:**

1. **Parse user input:**
   - Check if user message contains a quoted string - this is the commit message
   - Extract any scope keywords: "critical", "fp", or filenames
   - If commit message provided, plan to commit after fixes
   - Examples:
     - `/ssb-fix-review "refactor parsing logic"` → fix all, commit with that message
     - `/ssb-fix-review fp "apply parse don't validate"` → fix FP issues, commit
     - `/ssb-fix-review src/Parser.hs "improve parser types"` → fix that file, commit

2. **Identify what to fix:**
   - Look back at the most recent `/ssb-review` command output in this conversation
   - If no recent review exists, inform user and suggest running `/ssb-review` first
   - Determine scope based on extracted keywords:
     - "critical" → Focus on critical issues and bugs only
     - "fp" → Focus on functional programming principle violations
     - filename → Fix issues in that specific file only
     - Otherwise → Address all issues systematically

3. **Prioritize fixes in this order:**
   
   **Priority 1 - Critical Correctness:**
   - Security vulnerabilities
   - Logic errors and bugs
   - Partial function usage
   - Incomplete pattern matches
   - Type safety violations
   
   **Priority 2 - Testing:**
   - Add missing tests for new/modified code
   - Add property tests for pure functions
   - Add unit tests for edge cases and error conditions
   - Test algebraic laws and invariants
   - Fix brittle or incorrect tests
   
   **Priority 3 - FP Principles:**
   
   *Simplicity, Elegance, and Clarity (PARAMOUNT):*
   - Simplify complex code to make it obvious
   - Remove unnecessary abstraction
   - Make the intent immediately clear
   - Prefer boring, straightforward solutions
   - Ask: "Is there a simpler way?"
   
   *Parse, Don't Validate:*
   - Replace validation predicates with parsing into newtypes
   - Create smart constructors that return `Maybe` or `Either`
   - Wrap validated values in types so validation can't be bypassed
   - Add property tests for round-trip properties
   - Example transformation:
```haskell
     -- Before: validation
     validateEmail :: String -> Bool
     validateEmail = ...
     
     -- After: parsing with tests
     newtype Email = Email Text
       deriving (Eq, Show)
     
     parseEmail :: Text -> Maybe Email
     parseEmail txt = if isValid txt then Just (Email txt) else Nothing
     
     renderEmail :: Email -> Text
     renderEmail (Email txt) = txt
     
     -- Property test
     prop_emailRoundTrip :: Email -> Property
     prop_emailRoundTrip e = parseEmail (renderEmail e) === Just e
```
   
   *Purity and Effect Isolation:*
   - Extract pure functions from impure ones
   - Move IO to the edges of the system
   - Make side effects explicit in types
   - Use appropriate effect abstractions (ReaderT, StateT, ExceptT)
   - Test pure functions with property tests
   
   *Function Decomposition:*
   - Break large functions (>30 lines) into smaller, named pieces
   - Extract helper functions with clear, single purposes
   - Use `where` clauses for local helpers
   - Flatten deeply nested conditionals
   - Each small function becomes independently testable
   - Example:
```haskell
     -- Before: monolithic
     processRequest req = do
       let validated = validateInput req
       if validated
         then do
           result <- complexComputation req
           formatted <- formatResult result
           return formatted
         else return errorResponse
     
     -- After: decomposed and clear
     processRequest req = do
       input <- parseInput req
       result <- computeResult input
       return $ formatResult result
     
     parseInput :: Request -> Maybe ValidInput
     computeResult :: ValidInput -> IO Result
     formatResult :: Result -> Response
```
   
   *Compositionality:*
   - Identify repeated patterns and extract combinators
   - Make functions more general and reusable
   - Use function composition (`.`) and application (`$`, `&`)
   - Factor out common structure
   - Prefer composition over complex logic
   
   *Denotational Design & Algebraic Laws:*
   - Make illegal states unrepresentable with better types
   - Replace sentinel values with Maybe/Either
   - Use sum types instead of booleans for richer semantics
   - Create newtypes for domain concepts
   - **Identify and document algebraic laws**
   - **Add property tests for laws and invariants**
   - Example:
```haskell
     -- Before: invalid states possible
     data User = User
       { userId :: Int
       , userName :: Maybe String  -- can be Nothing after "login"?
       , isActive :: Bool
       , lastLogin :: Maybe UTCTime
       }
     
     -- After: illegal states impossible, with laws
     newtype UserId = UserId Int
       deriving (Eq, Ord, Show)
     
     newtype UserName = UserName Text
       deriving (Eq, Show)
     
     data UserState 
       = Active { lastLogin :: UTCTime }
       | Inactive
       deriving (Eq, Show)
     
     data User = User
       { userId :: UserId
       , userName :: UserName
       , userState :: UserState
       }
       deriving (Eq, Show)
     
     -- If User forms a Semigroup, test the laws:
     -- prop_userAssoc :: User -> User -> User -> Property
     -- prop_userAssoc a b c = (a <> b) <> c === a <> (b <> c)
```
   
   **Priority 4 - Code Quality:**
   - Remove code duplication
   - Improve naming
   - Add missing documentation
   - Performance improvements
   - Space leak fixes

4. **Implementation approach:**
   
   **For each issue to fix:**
   - Read the relevant file(s)
   - Explain what you're fixing and why
   - Show the before/after if significant
   - Make the change using appropriate tools
   - Verify the fix maintains or improves type safety
   - **Always ask: "What is the simplest way to do this?"**
   
   **When applying FP transformations:**
   - Start with type design - get the types right first
   - Identify algebraic laws and invariants
   - Use the type system to guide the implementation
   - Prefer total functions over partial ones
   - Compose small functions rather than writing large ones
   - Keep pure and impure code separated
   - Use newtype wrappers liberally for type safety
   - Make constraints explicit in types
   - Favor clarity over cleverness
   
   **When adding tests:**
   - For pure functions: add property tests
   - For parsers: add round-trip properties
   - For algebraic operations: test the laws
   - For edge cases: add unit tests
   - Make test names descriptive
   - Example property tests:
```haskell
     -- Semigroup laws
     prop_assoc :: Config -> Config -> Config -> Property
     prop_assoc a b c = (a <> b) <> c === a <> (b <> c)
     
     -- Monoid laws
     prop_leftIdentity :: Config -> Property
     prop_leftIdentity a = mempty <> a === a
     
     -- Round-trip
     prop_parseRender :: Value -> Property
     prop_parseRender v = parseValue (renderValue v) === Right v
     
     -- Invariant preservation
     prop_sortedStaysSorted :: [Int] -> Int -> Property
     prop_sortedStaysSorted xs x = 
       let sorted = sort xs
       in isSorted sorted ==> isSorted (insert x sorted)
```

5. **After completing fixes:**
   
   **If commit message was provided:**
   - Stage all modified files: `git add <files>`
   - Commit with the provided message: `git commit -m "<message>"`
   - Confirm the commit was successful
   
   **If no commit message provided:**
   - Just summarize changes, don't commit
   
   Provide a summary:
```
   ✅ Review Issues Fixed
   
   Critical Issues Resolved: X
   - [Brief description of each]
   
   Tests Added/Fixed:
   - Property tests: [what laws/invariants are tested]
   - Unit tests: [what edge cases are covered]
   - Coverage: [what is now tested]
   
   FP Improvements Applied:
   - Simplicity: [what was simplified]
   - Parse, Don't Validate: [what was changed]
   - Purity: [what was made pure/explicit]
   - Decomposition: [what was split up]
   - Type Safety: [what types were added/improved]
   - Algebraic Laws: [what laws were identified and tested]
   
   Files Modified:
   - file1.hs
   - file2.hs
   
   [If committed:]
   ✓ Committed with message: "<message>"
   
   [If not committed:]
   Suggested Next Steps:
   - Run tests: `cabal test`
   - Review the changes
   - Verify property tests pass
   - Check test coverage
```

6. **FP refactoring patterns to apply:**

   **Pattern: Validation → Parsing (with tests)**
   - Replace `validate :: a -> Bool` with `parse :: a -> Maybe b`
   - Wrap result in newtype to prevent misuse
   - Use Either for error messages: `parse :: a -> Either Error b`
   - Add round-trip property test
   
   **Pattern: Large Function → Composition**
   - Identify logical steps in large function
   - Extract each step as a named function
   - Compose steps with `.`, `>=>`, `>>>`, etc.
   - Each extracted function is now testable
   
   **Pattern: Effects → Type System**
   - Replace hidden IO with explicit type signatures
   - Use mtl-style constraints for flexibility
   - Push IO to edges, keep core logic pure
   - Pure logic becomes property-testable
   
   **Pattern: Booleans → Sum Types**
   - Replace boolean flags with descriptive sum types
   - Makes intent clearer and adds type safety
   - Example: `Bool` → `data Status = Active | Inactive`
   
   **Pattern: Null/Undefined → Maybe/Either**
   - Replace nullable/undefined with Maybe
   - Use Either for error context
   - Make functions total
   
   **Pattern: Sentinel Values → Types**
   - Replace -1, "", null with proper types
   - Use NonEmpty for non-empty lists
   - Use Positive, NonZero from refined library
   
   **Pattern: Complex Logic → Simple Algebra**
   - Identify the algebraic structure
   - Define operations that follow laws
   - Test the laws with property tests
   - Let the algebra guide the implementation

7. **Important notes:**
   - Always preserve existing tests and make them pass
   - If a fix would break existing API, note it and ask for confirmation
   - For large refactors, offer to do them incrementally
   - If unsure about a fix, explain options and ask for guidance
   - Balance perfection with pragmatism - don't over-engineer
   - **Always prefer the simpler solution**
   - **If you can't explain it simply, simplify it**
   - When committing, ensure all files compile and tests pass first

**Philosophy:**
- Simple is better than complex, complex is better than complicated
- Clarity and obviousness are features, not bugs
- The best code is code that doesn't need to be clever
- Types are your friends - use them to enforce correctness
- Parse at boundaries, trust types internally
- Small, pure, composable functions over large, impure, monolithic ones
- **If it's not tested, it's broken**
- **Test the laws, not just examples**
- **Identify the algebra - what are the laws and invariants?**
- Make the compiler catch bugs, not users
- If it compiles and types are right, it's more likely to be correct
- Refactor toward better types, simpler code, and clearer composition
- Always ask: "Is there a simpler way to do this?"
- Favor boring, obvious solutions over clever ones
