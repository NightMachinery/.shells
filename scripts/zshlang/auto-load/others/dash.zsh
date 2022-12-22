function ffdash {
    dasht-query-line $@ | perl -0777 -pe 's/name = (.*?)\ntype = (.*?)\nfrom = (.*?)\nurl = (.*?)\n/ $4 | $1 | $3 | $3\n\n/g' | column -ts'|' | fz --no-sort --with-nth=2.. | awkn 1 | inargsf w3m

    # @upstreamBug https://github.com/sunaku/dasht/issues/37 dasht-query-line has a limit on the results it returns
}
# function ffdash-i() {
#     FZF_DEFAULT_COMMAND=echo fz-empty --reverse --bind "change:reload:dasht-query-line {q} || true" --disabled --query "" --print-query | ghead -n 1
# }

alias hh='ffdash'
##
