@/Users/jfilipe/.codex/RTK.md

## Package ownership

- Install every package through Homebrew when a formula or cask is available.
- Use Nix packages only when Homebrew does not provide the package. Verify
  availability before adding a Nix dependency, and migrate it to Homebrew when
  Homebrew gains support.
- This applies to packages used by Home Manager `programs.*` modules too.
  Managing configuration with Nix does not justify installing a Nix binary.
  Disable the module's package installation or manage configuration/integration
  separately when necessary.
- Avoid duplicate Homebrew/Nix installations of the same tool. Shell hooks,
  services, and interactive commands must use the same Homebrew executable.
- Existing Nix-managed packages with Homebrew equivalents are migration debt,
  not exceptions to this policy. Preserve application data during migration.
