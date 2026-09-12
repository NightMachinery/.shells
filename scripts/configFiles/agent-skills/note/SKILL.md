---
name: note
description: Turn conversation findings or supplied material into a concise note in an appropriate location under ~/notes. Use when asked to note, remember, or save something there; discover the destination with a cheap subagent and confirm the exact path and draft before writing.
---

# Note

Preserve the useful result, not the conversation transcript. Default to
`~/notes` unless the user names another notes root. Keep drafting and final
review with the parent; delegate destination discovery to a cheap worker.

## Draft while the worker finds a home

Distill the relevant context into a short note: the finding or decision,
essential evidence, and actionable fix or next step. Include a date for an
incident when useful. Preserve the user's latest corrections and precise
actions (for example, force-close and reopen versus toggle a connection).
Separate observed facts, user-confirmed outcomes, and unconfirmed causes.
Usually a short paragraph or a few bullets suffice. Omit the investigative
transcript, failed permission attempts, and incidental personal details.

When destination discovery is needed, delegate it explicitly:

- **Codex:** use **Terra**, model `gpt-5.6-terra`, with fresh context
  (`fork_turns="none"` when using `spawn_agent`).
- **Claude:** select **Sonnet** through the runtime's available subagent model
  selector; do not inherit the parent's model or guess an unsupported model ID.

Give the worker a compact topic summary, notes root, user constraints, and this
read-only search brief. Do not fork the whole conversation or ask it to write
the note. While it searches, prepare the draft locally.

> Find the best existing home for this note. Read applicable AGENTS.md files.
> Start with directory names and `rg --files`; use topic synonyms to narrow
> candidate paths. Search product/topic and platform/device names independently
> across the notes root before choosing a hierarchy; do not stop at the first
> plausible folder. Then read relevant headings and nearby text. Avoid broad
> full-content searches, backups, exports, and unrelated sensitive material.
> Prefer an existing matching note or section; otherwise propose a file in an
> established topic folder. Follow nearby naming and format conventions.
> Return at most three ranked exact paths, existing/proposed status, target
> heading if applicable, short evidence for fit, and any duplicate or privacy
> concern. Do not write, stage, commit, or recursively delegate.

If the user already supplies the destination, inspect that location without
an unnecessary search worker. If the requested worker or model selection is
unavailable, say so briefly and do a narrow local search; do not silently
substitute a more expensive model.

## Propose a concrete note

Review the worker's evidence and inspect the best candidate. Match its format
(often Org), heading structure, and applicable instructions. Prefer a relevant
existing section over a duplicate note. Use the private tree for personal
device, account, or incident details; a sanitized general lesson may fit the
public tree. Do not assume that a directory named private proves its Git remote
is private.

If the note is already recorded accurately, return its link without rewriting
it or requesting approval for a no-op.

Show the exact proposed destination, whether creating or appending/updating,
and the complete concise draft. Give a brief reason for the location; explain
an alternative's tradeoff only when it would materially affect the choice.

**Wait for explicit confirmation of the destination and draft before writing.**
Make confirmation the final step after discovery and drafting, not a request
to begin investigating. Do not create the destination, write a provisional
note, or make companion edits in the notes tree before confirmation. A reply
such as "good" or "yes" approves the latest concrete proposal; incorporate
corrections, and do not ask again for a destination already approved in the
conversation. A correction alone is not approval. A later instruction that
explicitly waives confirmation takes precedence.

## Save the approved result

Re-read the target before editing to detect intervening changes. Save only the
approved note or section and verify its content. Follow the notes repository's
instructions for links, commits, and pushing, using explicit pathspecs and
preserving unrelated work. Stay on its current branch; do not create a Git
repository. Avoid copying the note into unrelated project docs or creating
extra index entries unless required or included in the approved proposal.

Finish with a clickable link to the saved file and a short, accurate statement
of commit/push status if relevant. If saving is blocked, distinguish the
prepared draft from a saved note.
