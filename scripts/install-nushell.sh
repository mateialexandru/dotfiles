#!/usr/bin/env bash
# Add the public and optional private Nu configuration through user autoload.
# Deliberately does not edit config.nu, .zshrc, $SHELL, or terminal defaults.

set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"

if ! command -v nu >/dev/null 2>&1; then
    echo "Error: nu is not installed." >&2
    exit 1
fi

AUTOLOAD_DIR="$(nu -n -c '$nu.user-autoload-dirs | first')"
mkdir -p "$AUTOLOAD_DIR"

link_config() {
    local source=$1 target=$2
    if [[ -L "$target" ]]; then
        ln -sfn "$source" "$target"
        echo "Relinked $target -> $source"
    elif [[ -e "$target" ]]; then
        echo "WARNING: $target exists and is not a symlink; leaving it alone."
    else
        ln -s "$source" "$target"
        echo "Linked $target -> $source"
    fi
}

link_config "$DOTFILES_DIR/config/nushell/dotfiles.nu" "$AUTOLOAD_DIR/10-dotfiles.nu"

if command -v zoxide >/dev/null 2>&1; then
    zoxide init nushell > "$AUTOLOAD_DIR/20-dotfiles-zoxide.nu"
    echo "Generated Nushell zoxide integration."
fi

if command -v fzf >/dev/null 2>&1; then
    fzf --nushell > "$AUTOLOAD_DIR/30-dotfiles-fzf.nu"
    echo "Generated Nushell fzf integration."
fi

# Nu source paths are parse-time values, so private layers join the same native
# autoload mechanism rather than being sourced dynamically from dotfiles.nu.
LAYERS_DIR="$HOME/.config/dotfiles/layers.d"
find "$AUTOLOAD_DIR" -maxdepth 1 -type l -name '50-layer-*.nu' -delete
if [[ -d "$LAYERS_DIR" ]]; then
    for layer in "$LAYERS_DIR"/*; do
        [[ -d "$layer" && -f "$layer/shell/init.nu" ]] || continue
        name="$(basename "$layer")"
        link_config "$layer/shell/init.nu" "$AUTOLOAD_DIR/50-layer-$name.nu"
    done
fi

echo "Nushell config ready. Run 'nu' to try it; your login shell is unchanged."
