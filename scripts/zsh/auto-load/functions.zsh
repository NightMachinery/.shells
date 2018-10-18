function pbadd() {
    osascript ~/'scripts/applescript/path-copy.applescript' "${(f)$(re realpath $@)}" > /dev/null
}
function cpsd() {
    local i;
    local counter=1;
    local outs=();
    for i in "$@"; do
        local B=$(basename "$i"); local D=$(dirname "$i");
        local out="$D/${B%.*}.png"
        convert "$i""[0]" "$out"
        outs[$counter]=$out
        counter=$(($counter + 1))
    done
    pbadd "${(@)outs}"
}
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
