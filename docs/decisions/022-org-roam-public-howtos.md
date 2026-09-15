# ADR-022: One roam graph, repository-owned how-tos

## Context

Operational notes such as how to use gptel, Mermaid, or Excalidraw describe
capabilities provisioned by this repository. Keeping copies in a private
knowledge repository makes those notes drift and prevents fresh personal or work
profiles from discovering them. Personal machine details and private knowledge
must not move into the public repository.

Org-roam natively accepts one `org-roam-directory`, while the desired graph has
one writable private root plus reusable public notes from this checkout.

## Decision

Each roam context keeps its primary directory and database. The public Doom
configuration extends Org-roam's file discovery and membership checks with
`my/roam-extra-directories`; `docs/howto/` is registered there by default.

- Captures and dailies continue to use the context's primary directory.
- Notes under `docs/howto/` are normal Org-roam nodes with stable IDs and the
  tags `howto` and `dotfiles`.
- Private profiles may add other roots, but this repository never names them.
- Private notes may link to public nodes. Public nodes must not link to private
  IDs; private backlinks remain visible without leaking into Git.

## Consequences

- Personal and work contexts share the same provisioned operating knowledge.
- A how-to is versioned beside the configuration that makes it true.
- Editing a public how-to through either org-roam or its real checkout path
  updates the same database; no symlink overlay is required.
- Org-roam upgrades must retain the small multi-root adapter until native
  multiple-root support exists.
