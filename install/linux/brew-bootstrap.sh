#!/bin/bash
# Homebrew bootstrap — source this file, then call brew_bootstrap explicitly.
# Sourcing alone has NO side effects. macOS only; no-op on every other OS.
: "${DOTFILES_DIR:=$(cd "$(dirname "$0")/../.." && pwd)}"
# shellcheck source=/dev/null
source "$DOTFILES_DIR/bin/detectos.sh"
# shellcheck source=/dev/null
source "$DOTFILES_DIR/bin/colors.sh"

brew_bootstrap() {
    # shellcheck source=/dev/null
    source "$DOTFILES_DIR/bin/detectos.sh"

    if [[ "$OSDIST" != "macos" ]]; then
        return 0
    fi

    echo ""
    printf "${C_BOLD}--- Bootstrapping Homebrew ---${C_RESET}\n"

    # Warm path: brew already installed — apply shellenv for this process.
    if [[ -x /opt/homebrew/bin/brew ]]; then
        eval "$(/opt/homebrew/bin/brew shellenv)"
    elif [[ -x /usr/local/bin/brew ]]; then
        eval "$(/usr/local/bin/brew shellenv)"
    else
        # Cold path: brew genuinely absent.
        if ! /usr/bin/pgrep -q oahd 2>/dev/null; then
            echo "Installing Rosetta..."
            softwareupdate --install-rosetta --agree-to-license
        else
            echo "Rosetta is already installed."
        fi

        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

        if [[ "$ISM1" == "true" ]]; then
            eval "$(/opt/homebrew/bin/brew shellenv)"
        else
            eval "$(/usr/local/bin/brew shellenv)"
        fi
    fi
}
