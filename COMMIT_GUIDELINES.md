# Git Commit Organization Guidelines for Dotfiles Repository

When organizing your changes into commits, consider grouping related modifications together for better maintainability and understanding.

## Commit Organization Strategy:

### 1. Documentation Updates
Commit all documentation-related changes together:
- CLAUDE.md (repository overview and workflow details)
- README.md (user-facing install instructions)
- decisions/014-gptel-emacs-llm-client.md

### 2. Doom Emacs Configuration Changes
Group all Doom Emacs configuration modifications:
- doom/config.el (core config)
- doom/config-gptel.el (LLM client settings)
- doom/packages.el (package declarations)

### 3. Scripts and Tooling Updates
Update scripts that support the environment:
- scripts/keeper-health.sh

### 4. New Features and Examples
Add new functionality or examples together:
- decisions/016-mermaid-previews.md (new feature documentation)
- doom/mermaid-config.json (configuration for new feature)
- examples/ (example files demonstrating new capability)

## Commit Message Format:

Use a clear, descriptive commit message format:
```
feat: Add mermaid preview support to Doom Emacs

This commit adds support for mermaid previews in Doom Emacs by:
- Adding mermaid-org-example.org and .svg files
- Including mermaid-markdown-example.md
- Configuring mermaid-preview functionality in doom/mermaid-config.json
- Updating documentation in decisions/016-mermaid-previews.md

Fixes #issue-number (if applicable)
```

## Best Practices:

- Keep related changes together in single commits
- Use imperative tense in commit messages
- Ensure each commit represents a complete, testable change
- Avoid mixing unrelated changes in the same commit
- Write clear descriptions of what was changed and why

This approach will make it easier to track changes and understand your modifications when reviewing history.
