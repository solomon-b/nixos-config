# Writing style

Write all prose in ASD-STE100 Simplified Technical English. Apply it to chat replies, code comments, docstrings, commit messages, PR bodies, and changelog entries. Where STE and the rules below disagree, follow STE.

STE rules:

- Keep sentences short. Instructions: 20 words maximum. Descriptions: 25 words maximum.
- Put one instruction in one sentence.
- Use the active voice.
- Use simple verb tenses only: present, past, and future.
- Do not use a gerund when a simple verb works.
- Give each word one meaning. Do not use the same word as a noun and as a verb.
- Do not use contractions.
- Do not put more than 3 nouns together.
- Write positive statements. Do not put two negatives in one sentence.
- Use a vertical list when the data has more than two parts.
- Keep paragraphs to 6 sentences maximum.
- Do not use slang, idioms, or figures of speech.

Do not change technical names. Identifiers, file names, table names, column names, and function names stay as they are.

Write like a person, not an AI. Avoid the usual tells.

Do not use em-dashes. Use a comma or a period, or rewrite the sentence.

Do not splice clauses together with colons or semicolons in the middle of a sentence. Prefer short, separate sentences.

Do not overuse bold, italics, or bullet lists. Default to plain prose and reach for a list only when the content is genuinely a list.

Avoid clichéd filler and AI phrases such as "load-bearing", "it's worth noting", "that said", "at the end of the day", "delve", "seamless", "robust", "leverage", and "crucial".

Keep it direct and concrete.

# How I program

Before you write, design, or review code, load the `solomon-style` skill. It has the full conventions.

The short version:

- Design from the types. Write an informal spec, then find the smallest type that fits it.
- Find the lawful structure. Reuse an existing abstraction before you write an ad hoc function.
- Build narrow components and combine them.
- Keep syntax separate from interpretation. Adapt to protocols at the edge.
- Put invariants in types, but hide advanced type machinery behind a clean interface.
- Use the least powerful tool that still permits static checks.
- Start with a naive version. Fix one defect at a time.
- Follow the conventions of the repo that you are in. Where the repo has no convention, follow the skill.
