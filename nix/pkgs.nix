import (builtins.fetchGit {
  # Descriptive name to make the store path easier to identify
  name = "nixos-unstable";
  url = "https://github.com/nixos/nixpkgs";
  # Commit hash for nixos-unstable as of 2020-10-13
  # `git ls-remote https://github.com/nixos/nixpkgs nixpkgs-unstable`
  ref = "refs/heads/nixpkgs-unstable";
  rev = "51bb9f3e9ab6161a3bf7746e20b955712cef618b";
})
