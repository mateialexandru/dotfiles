# Private layer example

The public configuration works without a layer. To add configuration from a
private repository, copy this layout into that repository and register it with
an ordered directory symlink:

```bash
mkdir -p "$HOME/.config/dotfiles/layers.d"
ln -sfn "/path/to/private/repo/dotfiles-profile" \
  "$HOME/.config/dotfiles/layers.d/10-private"
```

Every registered layer may provide:

- `doom/pre.el` for paths, contexts, feature flags, and other early values.
- `doom/post.el` for commands, keybindings, and late overrides.
- `shell/init.zsh` for private shell extensions.

Layers load in lexical order. Keep credentials in the operating-system
keychain, `auth-source`, or environment—not in a private Git repository.
