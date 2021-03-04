function h2e-stdin-bg() {
local title="${1:-untitled}" h2e_input="$(cat)"
local i="$(gmktemp --suffix .html)"
ec $h2e_input > $i
#awaysh-named safari2kindle reval-tlg h2e "$title" $i
awaysh-named safari2kindle reval-tlg fnswap full-html2 'ec $h2e_input' tlrl-ng "$2"
}
