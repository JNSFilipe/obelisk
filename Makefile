FLAKE  = $(HOME)/Documents/GitHub/obelisk\#gauss

# ── Core ──────────────────────────────────────────────────────────────────────

switch: ## Build and activate the system configuration
	darwin-rebuild switch --flake "$(FLAKE)"

build: ## Build without activating (dry run)
	darwin-rebuild build --flake "$(FLAKE)"

check: ## Evaluate the flake and run checks (no build)
	nix flake check

# ── Updates ───────────────────────────────────────────────────────────────────

update: ## Update all flake inputs (nixpkgs, home-manager, nix-darwin)
	nix flake update

upgrade: update switch ## Update inputs and activate in one step

# ── History ───────────────────────────────────────────────────────────────────

rollback: ## Roll back to the previous generation
	darwin-rebuild switch --rollback

generations: ## List all system generations
	darwin-rebuild --list-generations

# ── Cleanup ───────────────────────────────────────────────────────────────────

gc: ## Garbage-collect old nix store paths (keeps 7 days)
	nix-collect-garbage --delete-older-than 7d

gc-all: ## Garbage-collect ALL unused nix store paths
	nix-collect-garbage -d

store-size: ## Show nix store disk usage
	du -sh /nix/store

# ── Diagnostics ───────────────────────────────────────────────────────────────

diff: ## Show what changed between current and built config
	darwin-rebuild build --flake "$(FLAKE)" && nix store diff-closures /nix/var/nix/profiles/system ./result

packages: ## List all nix-managed packages in the current profile
	nix profile list --profile /nix/var/nix/profiles/system 2>/dev/null || echo "Use: nix-store -q --references /nix/var/nix/profiles/system"

brew-orphans: ## List homebrew formulae not declared in homebrew.nix
	brew list --formula

# ── Submodules ────────────────────────────────────────────────────────────────

doom-update: ## Pull latest Doom Emacs
	git submodule update --remote configs/emacs

# ── tmux ──────────────────────────────────────────────────────────��───────────

tpm-install: ## Install tmux plugin manager
	git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

# ── Bootstrap (first-time only) ──────────────────────────────────────────────

bootstrap: ## First-time nix-darwin install (run after installing nix)
	nix --extra-experimental-features "nix-command flakes" run nix-darwin -- switch --flake "$(FLAKE)"

uninstall-nix: ## Completely remove nix from the system
	/nix/nix-installer uninstall

# ── Help ──────────────────────────────────────────────────────────────────────

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## ' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2}'

.DEFAULT_GOAL := help
.PHONY: switch build check update upgrade rollback generations gc gc-all store-size diff packages brew-orphans doom-update tpm-install bootstrap uninstall-nix help
