if command -v opam &> /dev/null; then  eval $(opam env); fi
if [ -e /Users/evar/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/evar/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

psource "$HOME/.privateShell"
