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

# ── Bootstrap (first-time only) ──────────────────────────────────────────────

bootstrap: ## First-time nix-darwin install (run after installing nix)
	nix --extra-experimental-features "nix-command flakes" run nix-darwin -- switch --flake "$(FLAKE)"

uninstall-nix: ## Completely remove nix from the system
	/nix/nix-installer uninstall

# ── Help ──────────────────────────────────────────────────────────────────────

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## ' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2}'
	@printf "\n\033[1mWorkflows:\033[0m\n"
	@printf "  \033[33mEdit nix config and apply\033[0m\n"
	@printf "    edit nix/*.nix → make switch\n\n"
	@printf "  \033[33mEdit app config (live symlink)\033[0m\n"
	@printf "    edit configs/*  (takes effect immediately)\n\n"
	@printf "  \033[33mAdd a new CLI tool\033[0m\n"
	@printf "    edit nix/packages.nix → make switch\n\n"
	@printf "  \033[33mAdd a new GUI app\033[0m\n"
	@printf "    edit nix/homebrew.nix → make switch\n\n"
	@printf "  \033[33mAdd a new config file\033[0m\n"
	@printf "    add to configs/ → add home.file in nix/home.nix → make switch\n\n"
	@printf "  \033[33mUpdate everything\033[0m\n"
	@printf "    make upgrade  (= update + switch)\n\n"
	@printf "  \033[33mUpdate Doom Emacs\033[0m\n"
	@printf "    make doom-update → doom sync\n\n"
	@printf "  \033[33mSafe test before applying\033[0m\n"
	@printf "    make build → make diff → make switch\n\n"
	@printf "  \033[33mSomething broke after switch\033[0m\n"
	@printf "    make rollback\n\n"
	@printf "  \033[33mReclaim disk space\033[0m\n"
	@printf "    make gc  (or gc-all for aggressive cleanup)\n\n"
	@printf "  \033[33mAudit homebrew drift\033[0m\n"
	@printf "    make brew-orphans  (shows formulae not in homebrew.nix)\n"

.DEFAULT_GOAL := help
.PHONY: switch build check update upgrade rollback generations gc gc-all store-size diff packages brew-orphans doom-update bootstrap uninstall-nix help
