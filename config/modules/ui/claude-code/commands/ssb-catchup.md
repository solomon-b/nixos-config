# Catch Up on Recent Work

Get up to speed on recent development work in the branch, including Linear ticket context when available.

**Usage patterns:**
- `/ssb-catchup` - Analyze commits ahead of master/main (default)
- `/ssb-catchup wip` - Show staged and unstaged changes (work in progress)
- `/ssb-catchup 15` - Analyze last 15 commits
- `/ssb-catchup 30 develop` - Analyze commits ahead of develop branch

**Instructions:**

1. **Determine mode:**
   - If user message contains "wip", "staged", "unstaged", or "changes", show working directory changes
   - If a number is provided, analyze that many commits
   - Otherwise (default), analyze commits ahead of main branch

2. **Extract Linear ticket from branch name (all modes):**
   - Get current branch: `git branch --show-current`
   - Extract ticket ID by matching pattern `[A-Z]+-[0-9]+` from branch name (e.g., `solomon/feat/ENG-2267/fund-allocation-pool-api` → `ENG-2267`)
   - If a ticket ID is found:
     1. Use `ToolSearch` to search for Linear MCP tools (query: `+linear`, max_results: 10)
     2. If Linear tools are available, use them to fetch the issue details (title, description, status, assignee, priority, labels, comments)
     3. If Linear tools are NOT available, note the ticket ID and mention that the Linear plugin isn't connected in this session — suggest the user check Linear manually
   - This step runs in parallel with the git analysis below

3. **For work-in-progress mode (wip):**
   - Run `git status` to see overall state
   - Run `git diff` to show unstaged changes
   - Run `git diff --cached` to show staged changes
   - If there are untracked files, list them
   - Summarize:
     - What files have been modified
     - Nature of changes (new features, fixes, refactoring)
     - Whether changes are staged or unstaged
     - Suggestions for commit organization if multiple concerns

4. **For commit history mode (default):**
   - If a number is provided, use that many commits: `git log -n <number>`
   - If no number, find commits ahead of main branch:
     - Check which main branch exists: `git branch -a | grep -E 'master|main'`
     - Get current branch: `git branch --show-current`
     - If on feature branch: `git log origin/master..HEAD` (or origin/main)
     - If base branch specified, use that instead

   - Run `git log` with `--oneline --graph --decorate`
   - For each commit (or sample if many): `git show <commit-hash> --stat`
   - For significant commits: `git show <commit-hash>`

   - Provide insights:
     - Main features/fixes implemented
     - Most frequently modified files/modules
     - Patterns (refactoring, testing, docs)
     - Breaking changes or architectural updates
     - Overall development direction
     - Commit authors if multiple

5. **Progress summary (combine git + Linear context):**
   After gathering both git history and Linear ticket info, provide a progress assessment:

   - **If Linear ticket is available:**
     - Show the ticket title, status, and priority
     - Compare the ticket description/requirements against what the commits have accomplished
     - List what appears to be **done** (requirements addressed by commits)
     - List what appears to be **remaining** (requirements not yet addressed, or mentioned in ticket but not in code)
     - List any **open questions** (ambiguities between ticket and implementation)
     - Give an overall progress assessment (e.g., "Core implementation complete, needs tests and frontend integration")

   - **If no Linear ticket (or not available):**
     - Infer progress from commit history alone
     - Look for patterns: are commits building toward a feature? fixing issues? refactoring?
     - Check for TODO/FIXME comments in changed files
     - Note any uncommitted work (run `git status` and `git stash list`)
     - Give a summary of where things stand and what likely comes next

6. **Additional helpful context:**
   - Show summary statistics (files changed, insertions, deletions)
   - Highlight any potential issues (merge conflicts, large changes, etc.)
   - Suggest concrete next steps based on the progress assessment

**Example outputs:**

*For default (branch commits with Linear ticket):*
```
Branch: solomon/feat/ENG-2267/fund-allocation-pool-api
Linear: ENG-2267 — "Fund Allocation Pool API" (In Progress, High priority)

Commits (4 ahead of master):
  362098e fix(clearinghouse): harden POST /pool/allocations handler
  400ccb4 feat(clearinghouse): add pool summary and transactions DB layer
  337340c feat(clearinghouse): add GET /pool and GET /pool/transactions endpoints
  835ef78 chore: regen frontend files

Progress:
  Done:
  - Database layer for pool summaries and transactions
  - GET /pool and GET /pool/transactions endpoints
  - POST /pool/allocations hardening
  - Frontend type generation

  Remaining (from ticket):
  - DELETE /pool/allocations endpoint
  - Pool balance validation on allocation
  - Integration tests

  Overall: Core read APIs and DB layer complete. Write path hardened.
  Missing delete endpoint and integration tests.

Files touched: 12 files, +847 / -23
Next steps: Implement DELETE endpoint, add integration tests
```

*For default (branch commits, no Linear):*
```
Branch: solomon/feat/ENG-2267/fund-allocation-pool-api
Linear: ENG-2267 (Linear not connected — check ticket manually)

Commits (4 ahead of master):
  ...

Progress (inferred from commits):
  - Built out fund allocation pool feature end-to-end
  - Database layer → API endpoints → frontend types
  - Recent commit hardens the POST handler (suggests stabilization phase)

  Uncommitted: 3 untracked files (scripts, docs)

Next steps: Review untracked files, consider tests
```

*For work-in-progress:*
```
Branch: solomon/feat/ENG-2267/fund-allocation-pool-api
Linear: ENG-2267 — "Fund Allocation Pool API" (In Progress)

Current Working Changes:

Unstaged Changes:
- src/Main.hs: Added new parseConfig function
- lib/Database.hs: Refactored connection pooling

Staged Changes:
- tests/ParserSpec.hs: Added test cases for edge cases

Untracked Files:
- docs/new-feature.md

Progress vs ticket:
  Working on config parsing — this addresses the "configuration flexibility"
  requirement from the ticket. Tests being added in parallel.

Summary: Active work on config parsing with tests staged for commit
```
