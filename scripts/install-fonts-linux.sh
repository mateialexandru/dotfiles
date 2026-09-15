#!/usr/bin/env bash
# install-fonts-linux.sh — Install coding/UI fonts to ~/.local/share/fonts on Linux.
#
# Designed for immutable hosts (Bluefin/rpm-ostree) where dnf at runtime is
# either unavailable or requires reboot. macOS handles the same fonts through
# Homebrew Cask in install.sh, so this script is a no-op there.
#
# Layout under FONT_DIR:
#   <Font>-NF/    Nerd-Font-patched zips from ryanoasis/nerd-fonts
#   Inter/        UI variable-pitch from rsms/inter
#   IBMPlexMono/  IBM/plex-mono latest release
#   IBMPlexSans/  IBM/plex-sans latest release

set -euo pipefail

if [[ "$OSTYPE" != "linux-gnu"* ]]; then
    echo "install-fonts-linux.sh: macOS uses brew casks; nothing to do."
    exit 0
fi

NF_VERSION="${NF_VERSION:-v3.4.0}"
INTER_VERSION="${INTER_VERSION:-v4.1}"
FONT_DIR="${FONT_DIR:-$HOME/.local/share/fonts}"

mkdir -p "$FONT_DIR"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

already_installed() {
    local dir="$1"
    [[ -d "$dir" && -n "$(ls -A "$dir" 2>/dev/null)" ]]
}

install_zip() {
    local label="$1" url="$2" target="$3"
    if already_installed "$target"; then
        echo "=> $label already installed at $target — skipping."
        return 0
    fi
    echo "=> Downloading $label..."
    local zip
    zip="$WORK/$(basename "$target").zip"
    curl -fsSL -o "$zip" "$url"
    mkdir -p "$target"
    unzip -qo "$zip" -d "$target"
    # Strip non-font files (READMEs, licenses, web/css subdirs).
    find "$target" -type f ! \( -iname "*.ttf" -o -iname "*.otf" \) -delete
    find "$target" -type d -empty -delete
}

# --- Nerd-Font-patched zips ---
for font in JetBrainsMono Iosevka FiraCode CascadiaCode VictorMono Monaspace; do
    install_zip "$font Nerd Font ($NF_VERSION)" \
        "https://github.com/ryanoasis/nerd-fonts/releases/download/$NF_VERSION/$font.zip" \
        "$FONT_DIR/$font-NF"
done

# --- Maple Mono Nerd Font (upstream ships its own NF build) ---
MAPLE_VERSION="${MAPLE_VERSION:-v7.9}"
install_zip "Maple Mono NF $MAPLE_VERSION" \
    "https://github.com/subframe7536/maple-font/releases/download/$MAPLE_VERSION/MapleMono-NF.zip" \
    "$FONT_DIR/MapleMono-NF"

# --- Inter (pinned) ---
install_zip "Inter $INTER_VERSION" \
    "https://github.com/rsms/inter/releases/download/$INTER_VERSION/Inter-${INTER_VERSION#v}.zip" \
    "$FONT_DIR/Inter"

# --- IBM Plex Mono / Sans (pinned; the IBM/plex monorepo tags individual
#     packages like `@ibm/plex-mono@1.1.0`, hence the URL-encoded path).
PLEX_MONO_VERSION="${PLEX_MONO_VERSION:-1.1.0}"
PLEX_SANS_VERSION="${PLEX_SANS_VERSION:-1.1.0}"

install_zip "IBM Plex Mono $PLEX_MONO_VERSION" \
    "https://github.com/IBM/plex/releases/download/%40ibm/plex-mono%40${PLEX_MONO_VERSION}/ibm-plex-mono.zip" \
    "$FONT_DIR/IBMPlexMono"

install_zip "IBM Plex Sans $PLEX_SANS_VERSION" \
    "https://github.com/IBM/plex/releases/download/%40ibm/plex-sans%40${PLEX_SANS_VERSION}/ibm-plex-sans.zip" \
    "$FONT_DIR/IBMPlexSans"

# --- Iosevka Aile / Etoile (latest from be5invis/Iosevka; version moves fast,
#     so resolve the asset URL via the GitHub releases API rather than pin).
fetch_iosevka_pkg() {
    local variant="$1" target="$2"  # variant: Aile, Etoile
    if already_installed "$target"; then
        echo "=> Iosevka $variant already installed at $target — skipping."
        return 0
    fi
    local url
    url="$(curl -fsSL https://api.github.com/repos/be5invis/Iosevka/releases/latest \
        | grep -oE '"browser_download_url": *"[^"]+\.zip"' \
        | cut -d'"' -f4 \
        | grep -E "PkgTTF-Iosevka${variant}-[0-9]" \
        | head -1)"
    if [[ -z "$url" ]]; then
        echo "(!) Could not resolve latest Iosevka $variant — skipping."
        return 0
    fi
    install_zip "Iosevka $variant" "$url" "$target"
}

fetch_iosevka_pkg Aile "$FONT_DIR/IosevkaAile"
fetch_iosevka_pkg Etoile "$FONT_DIR/IosevkaEtoile"

# --- Fira Sans (mozilla/Fira has no release assets; pull source tarball and
#     extract the OTF subdirectory).
FIRA_VERSION="${FIRA_VERSION:-4.202}"
fira_target="$FONT_DIR/FiraSans"
if already_installed "$fira_target"; then
    echo "=> Fira Sans already installed at $fira_target — skipping."
else
    echo "=> Downloading Fira Sans $FIRA_VERSION..."
    fira_zip="$WORK/Fira.zip"
    curl -fsSL -o "$fira_zip" \
        "https://github.com/mozilla/Fira/archive/refs/tags/${FIRA_VERSION}.zip"
    mkdir -p "$fira_target"
    # Flat-extract any FiraSans*.otf nested under the source tree.
    unzip -j -o "$fira_zip" "*FiraSans*.otf" -d "$fira_target" >/dev/null 2>&1 || true
    if [[ -z "$(ls -A "$fira_target" 2>/dev/null)" ]]; then
        echo "(!) Fira Sans extraction yielded no files — removing empty dir."
        rmdir "$fira_target" 2>/dev/null || true
    fi
fi

echo "=> Refreshing font cache..."
fc-cache -f "$FONT_DIR" >/dev/null
echo "=> Fonts ready under $FONT_DIR."
