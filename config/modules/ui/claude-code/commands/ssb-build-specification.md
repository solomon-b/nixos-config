---
description: Create detailed technical specification for current branch
---

# /ssb-build-specification Command

## Overview
The `/ssb-build-specification` command helps create detailed technical specifications for features or changes in a git repository. It creates a branch-specific markdown file and guides you through an interactive specification development process.

## Command Workflow

### 1. Initial Setup
- **Verify git repository**: Check that the command is run within a git repository
- **Get current branch name**: Retrieve the active git branch
- **Create specification file**: Create `.claude/dev/${BRANCH}/PLAN.md` with sanitized branch name (special characters replaced with underscores)

### 2. Interactive Specification Development
The command initiates a conversation to gather:
- **Feature/Change Overview**: High-level description of what needs to be built
- **Requirements**: Functional and non-functional requirements
- **Technical Approach**: Implementation strategy and architecture
- **Components**: List of files, modules, or components to create/modify
- **Success Criteria**: How to validate the implementation
- **Edge Cases**: Special scenarios to consider
- **Dependencies**: External libraries, APIs, or other requirements

### 3. Detail Evaluation
After drafting the initial specification, the command will:
- **Review each section** systematically
- **Identify gaps** in detail or clarity
- **Ask clarifying questions** for ambiguous areas
- **Suggest additions** for completeness
- **Ensure measurable criteria** are defined

### 4. Implementation Step Planning
After completing the specification:
- **Distill the plan** into a sequence of self-contained implementation steps
- **Each step should be reviewable** as a single git commit by a human
- **Changes should be logical** and easily understood by reviewers
- **Steps can be independent** if that reduces overall complexity
- **Context should be preserved** in PLAN.md and any additional supporting files

### 5. File Generation
Once the specification is complete:
- **Write the final specification** to `.claude/dev/${BRANCH}/PLAN.md`
- **Write implementation steps** to `.claude/dev/${BRANCH}/TODO.md` for tracking across sessions
- **Include standard sections in PLAN.md**:
  - Overview
  - Requirements
  - Technical Design
  - Implementation Approach (high-level)
  - Testing Strategy
  - Success Metrics
  - Timeline (if applicable)
- **TODO.md should contain**:
  - Detailed step-by-step implementation tasks
  - Each step designed as a reviewable git commit
  - Dependencies noted where necessary

## Usage Example

```bash
# In a git repository on branch "feature/user-authentication"
/build-specification

# Creates: .claude/dev/feature_user_authentication/PLAN.md
# Starts interactive specification process
```

## File Naming Convention
- Branch: `feature/user-auth` → File: `.claude/dev/feature_user_auth/PLAN.md`
- Branch: `bugfix/issue-123` → File: `.claude/dev/bugfix_issue_123/PLAN.md`
- Branch: `release/v2.0.0` → File: `.claude/dev/release_v2_0_0/PLAN.md`

## Implementation Notes

### Step-by-step Process

1. **Check git repository**
   ```bash
   git rev-parse --git-dir
   ```

2. **Get current branch**
   ```bash
   git branch --show-current
   ```

3. **Sanitize branch name**
   - Replace `/`, `-`, `.`, and other special characters with `_`
   - Example: `feature/new-api.v2` → `feature_new_api_v2`

4. **Create initial file**
   ```bash
   echo "# ${BRANCH} Specification\n\n## Status: Draft\n" > .claude/dev/${SANITIZED_BRANCH}/PLAN.md
   ```

5. **Interactive conversation flow**
   - Start with: "Let's build a specification for this feature. What is the main goal or problem you're trying to solve?"
   - Progressively gather details for each section
   - Use prompting questions like:
     - "What are the key requirements?"
     - "Are there any performance considerations?"
     - "What existing code will this interact with?"
     - "How will you know when this is complete?"

6. **Detail evaluation checklist**
   - [ ] Is the scope clearly defined?
   - [ ] Are acceptance criteria measurable?
   - [ ] Are technical constraints identified?
   - [ ] Is the implementation approach specific?
   - [ ] Are testing requirements defined?
   - [ ] Are dependencies listed?
   - [ ] Are risks and mitigation strategies noted?

7. **Final specification template**
   ```markdown
   # [Branch Name] Specification

   ## Overview
   [High-level description]

   ## Requirements
   ### Functional Requirements
   - [Requirement 1]
   - [Requirement 2]

   ### Non-Functional Requirements
   - [Performance targets]
   - [Security considerations]

   ## Technical Design
   ### Architecture
   [Design decisions and patterns]

   ### Components
   - [Component 1]: [Description]
   - [Component 2]: [Description]

   ## Implementation Plan
   1. [Phase 1]
   2. [Phase 2]

   ## Testing Strategy
   - Unit tests: [Coverage areas]
   - Integration tests: [Scenarios]
   - Manual testing: [Procedures]

   ## Success Metrics
   - [Metric 1]
   - [Metric 2]

   ## Risks and Mitigations
   - Risk: [Description] → Mitigation: [Strategy]

   ## Timeline
   - Estimated effort: [Hours/Days]
   - Target completion: [Date]
   ```

## Error Handling
- **Not in git repository**: Display error message and exit
- **No branch detected**: Use "main" or "master" as default with warning
- **File already exists**: Ask user whether to overwrite or append
- **Write permissions**: Check file write permissions before starting

## Best Practices
- Keep specifications concise but complete
- Focus on "what" and "why" more than "how"
- Include acceptance criteria that can be tested
- Document assumptions explicitly
- Update the specification as requirements evolve

