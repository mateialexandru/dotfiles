#!/usr/bin/env bash
# Install Pi and compose its public configuration with activated private layers.

set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"
PI_HOME="${PI_CODING_AGENT_DIR:-$HOME/.pi/agent}"
LAYERS_DIR="$HOME/.config/dotfiles/layers.d"
PI_PACKAGE="@earendil-works/pi-coding-agent"
PI_VERSION="0.85.1"
PI_ACP_PACKAGE="pi-acp"
PI_ACP_VERSION="0.0.33"

current="$(pi --version 2>/dev/null | tr -d 'v[:space:]' || true)"
if [[ "$current" == "$PI_VERSION" ]]; then
    echo "Pi $current is already installed."
else
    npm install --global --ignore-scripts "$PI_PACKAGE@$PI_VERSION"
fi

pi_acp_current="$(
    npm list --global --depth=0 --json "$PI_ACP_PACKAGE" 2>/dev/null |
        node -e '
let input = "";
process.stdin.setEncoding("utf8");
process.stdin.on("data", chunk => input += chunk);
process.stdin.on("end", () => {
  try { process.stdout.write(JSON.parse(input).dependencies?.["pi-acp"]?.version ?? ""); }
  catch {}
});' || true
)"
if [[ "$pi_acp_current" == "$PI_ACP_VERSION" ]]; then
    echo "Pi ACP adapter $pi_acp_current is already installed."
else
    npm install --global --ignore-scripts "$PI_ACP_PACKAGE@$PI_ACP_VERSION"
fi

mkdir -p "$PI_HOME"

settings=("$DOTFILES_DIR/config/pi/settings.json")
models=("$DOTFILES_DIR/config/pi/models.json")
contexts=("$DOTFILES_DIR/config/pi/AGENTS.md")
hooks=()

if [[ -d "$LAYERS_DIR" ]]; then
    for layer in "$LAYERS_DIR"/*; do
        [[ -d "$layer" ]] || continue
        [[ -f "$layer/pi/settings.json" ]] && settings+=("$layer/pi/settings.json")
        [[ -f "$layer/pi/models.json" ]] && models+=("$layer/pi/models.json")
        [[ -f "$layer/pi/AGENTS.md" ]] && contexts+=("$layer/pi/AGENTS.md")
        [[ -f "$layer/pi/install.sh" ]] && hooks+=("$layer/pi/install.sh")
    done
fi

node "$DOTFILES_DIR/scripts/merge-json.mjs" "$PI_HOME/settings.json" "${settings[@]}"
node "$DOTFILES_DIR/scripts/merge-json.mjs" "$PI_HOME/models.json" "${models[@]}"

context_tmp="$(mktemp "${PI_HOME}/AGENTS.md.XXXXXX")"
for context in "${contexts[@]}"; do
    cat "$context" >> "$context_tmp"
    printf '\n' >> "$context_tmp"
done
mv "$context_tmp" "$PI_HOME/AGENTS.md"

if [[ "${PI_SKIP_LAYER_HOOKS:-0}" == 1 ]]; then
    echo "Skipping Pi layer setup hooks."
else
    for hook in "${hooks[@]}"; do
        echo "Applying Pi layer: $hook"
        bash "$hook"
    done
fi

echo "Pi configuration composed from ${#settings[@]} layer(s) in $PI_HOME."
