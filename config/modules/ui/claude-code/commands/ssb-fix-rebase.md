# Fix Rebase Conflicts

Intelligently resolve rebase conflicts by analyzing both sides of each conflict with codebase context. Auto-continues through high-confidence resolutions and pauses for human review only when unsure.

**Usage patterns:**
- `/ssb-fix-rebase` - Resolve conflicts in an active rebase (must already be mid-rebase)

**Instructions:**

1. **Verify rebase state:**
   - Run `git status` to confirm we're mid-rebase (look for "rebase in progress" or "You are currently rebasing")
   - If NOT mid-rebase, tell the user and stop. Do not start a rebase — only resolve conflicts in one already in progress
   - Identify which commit is being applied (from `.git/rebase-merge/message` or git status output)

2. **Parse conflicted files:**
   - From `git status`, collect all files marked as "both modified" or "both added" (UU, AA states)
   - If there are no conflicts but rebase is paused, run `git rebase --continue` and check if new conflicts appear

3. **For each conflicted file, analyze and resolve:**
   - Read the file to find all conflict markers (`<<<<<<<`, `=======`, `>>>>>>>`)
   - For each conflict region:
     - Identify the "ours" side (current branch, above `=======`) and "theirs" side (incoming commit, below `=======`)
     - Read surrounding code context (50+ lines above and below) to understand intent
     - Check git log for both sides to understand what each change was trying to accomplish
     - If helpful, read related files (imports, type definitions, callers) to understand which resolution is correct

4. **Classify each conflict's confidence level:**

   **High confidence** (auto-resolve without pausing):
   - Both sides add imports — merge both sets
   - Non-overlapping additions in the same region (e.g., both add different functions)
   - One side only has whitespace or formatting changes — take the substantive change
   - Identical changes on both sides — take either
   - One side adds code and the other doesn't touch that region — take the addition
   - Simple reordering of independent items (e.g., export lists)

   **Low confidence** (pause for review):
   - Both sides modify the same lines with different logic
   - One side deletes code the other side modifies
   - Changes to function signatures, type definitions, or data model fields
   - Conflicts in database migrations, config files, or lockfiles
   - Business logic changes where intent is ambiguous
   - Any conflict where the correct resolution isn't obvious from context

5. **Apply resolutions and continue:**
   - Write the resolved content to each conflicted file
   - Run `git add` on all resolved files
   - **If ALL conflicts were high-confidence:**
     - Show a brief one-line-per-file summary of what was resolved and how
     - Run `git rebase --continue` immediately
   - **If ANY conflict was low-confidence:**
     - Show a detailed summary for each low-confidence resolution:
       - The original conflict (both sides)
       - Your proposed resolution
       - Your reasoning
     - Ask the user: "Approve these resolutions? (yes / edit / abort)"
       - **yes**: Stage and `git rebase --continue`
       - **edit**: Let user specify changes, then re-stage and continue
       - **abort**: Run `git rebase --abort` and stop

6. **Loop until rebase completes:**
   - After `git rebase --continue`, check if new conflicts appear on the next commit
   - If yes, go back to step 2
   - If `git rebase --continue` fails for a non-conflict reason (e.g., test hook failure), report the error and stop — do not retry
   - When the rebase finishes successfully, show a final summary:
     - Number of commits replayed
     - Total conflicts resolved (high-confidence vs low-confidence)
     - List of files that were modified during conflict resolution

7. **Safety rules:**
   - NEVER start a rebase — only resolve conflicts in one already in progress
   - NEVER use `--force` or `--skip` options
   - If you cannot determine a correct resolution, classify it as low-confidence and ask
   - If the user says "abort" at any point, immediately run `git rebase --abort`
   - Do not modify files that don't have conflict markers
