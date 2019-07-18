function github-dir() {
    svn export "$(sed 's/tree\/master/trunk/' <<< "$1")" "$2"  
}
function rename-for-podcast0() {
    local c=1
    local i
    for i in "${@:3}"
             {
                 echo "$i to "  "$(dirname $i)/1-$(printf '%02d' $c) $2"
                 if test $1 = 1 ; then mv "$i" "$(dirname $i)/1-$(printf '%02d' $c) $2"; fi
                 c=$(($c + 1))
             }
}
function pbadd() {
    osascript ~/'scripts/applescript/path-copy.applescript' "${(f)$(re realpath $@)}" > /dev/null
}
function cpsd() {
    local i;
    local counter=1;
    local outs=();
    for i in "${@:2}"; do
        local B=$(basename "$i"); #local D=$(dirname "$i");
        local out="$1/${B%.*}.png"
        convert "$i""[0]" "$out"
        outs[$counter]="$out"
        printf '%s\n' "$out"
        counter=$(($counter + 1))
    done
    pbadd "${(@)outs}"
}
function cpsdi() {
    cpsd "$(dirname "$1")" "$@"
}
function cpsdt() {
    mkdir -p ~/tmp/delme
    cpsd ~/tmp/delme "$@"
}
function psd2telg() {
    cpsdt "$@"|xargs -I {} ensure-run.zsh 60s tsend me ' ' -f {} --force-document
}
function ls-by-added() {
    # Doesn't work well for files having '(null)' as their DateAdded, which some do.
    mdls -name kMDItemFSName -name kMDItemDateAdded -raw -- *(D) | \
        xargs -0 -I {} echo {} | \
        sed 'N;s/\n/ /' | \
        sort --reverse | \
        sed -E "s/^.*\\+0000 //" # removes the timestamps
}
# increment-episode() {
# superseded by tmnte
#   emulate -L zsh
#   setopt extendedglob
#   local cmd=${$(fc -nl -1 -1)/(#b)(*E)(<->)/$match[1]${(l:${#match[2]}::0:)$((match[2]+${1:-1}))}}
#   geval "$cmd"
# }
ins-npm() {
    zargs -l 1 -- $(cat ~/scripts/setup/node.g) -- npm install -g
}
ins-pip() {
    zargs -l 1 -- ~/scripts/python/**/requirements.txt -- pip install -U -r
}
ins-ins() {
    zargs -n 1 -- $(cat ~/scripts/setup/installables) -- ins #Don't quote the inputs, it makes zargs treat them as one monolithic input.
}
ins-linux() {
    zargs -n 1 -- $(cat ~/scripts/setup/installables-linux) -- ins #Don't quote the inputs, it makes zargs treat them as one monolithic input.
}
ins-brew() {
    brew bundle install --file=~/scripts/setup/brewables
}
