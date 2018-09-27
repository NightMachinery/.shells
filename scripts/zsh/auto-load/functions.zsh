function ls-by-added() {
    # Doesn't work well for files having '(null)' as their DateAdded, which some do.
    mdls -name kMDItemFSName -name kMDItemDateAdded -raw -- *(D) | \
        xargs -0 -I {} echo {} | \
        sed 'N;s/\n/ /' | \
        sort --reverse | \
        sed -E "s/^.*\\+0000 //" # removes the timestamps
}
increment-episode() {
  emulate -L zsh
  setopt extendedglob
  local cmd=${$(fc -nl -1 -1)/(#b)(*E)(<->)/$match[1]${(l:${#match[2]}::0:)$((match[2]+1))}}
  # local cmd=${$(fc -nl -1 -1)/(#b)E(<->)/$E${(l:${#match[1]}::0:)$((match[1]+1))}}
  # display it
  echo "$cmd"

  # put it on the history
  print -S -- "$cmd"

  # evaluate it
  eval -- "$cmd"
              }
