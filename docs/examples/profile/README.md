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
- `shell/init.nu` for private Nushell extensions. Rerun
  `scripts/install-nushell.sh` (or `.ps1`) after activating a new layer so it is linked
  into Nu's user autoload directory.
- `pi/settings.json` and `pi/models.json` for deep-merged Pi configuration.
- `pi/AGENTS.md` for private global agent context appended after the public context.
- `pi/install.sh` or `pi/install.ps1` for an optional platform-specific Pi setup hook.

Rerun `scripts/install-pi.sh` (or `.ps1`) after activating a layer. JSON objects
are merged in layer order; arrays in a later layer replace earlier arrays.

Layers load in lexical order. Keep credentials in the operating-system
keychain, `auth-source`, or environment—not in a private Git repository.
