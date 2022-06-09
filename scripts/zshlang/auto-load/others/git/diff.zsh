##
alias gd='git-diff'
alias gdca='git-diff --cached'
alias gdcw='git-diff --cached --word-diff'
alias gdct='git describe --tags `git rev-list --tags --max-count=1`'
alias gdt='git diff-tree --no-commit-id --name-only -r'
alias gdw='git-diff --word-diff'
##
function git-diff-kitty {
    bella_zsh_disable1

    # https://sw.kovidgoyal.net/kitty/kittens/diff.html
    # Needs some config in git

    # @kittyBug submodules do not work
    git difftool --tool='kitty' --submodule=diff --dir-diff "$@"
    # --dir-diff : concats all the files into a single diff.
}
##
function git-diff() {
    bella_zsh_disable1

    if (( $#@ == 0 )) ; then
        set -- 'HEAD~0'
        # without this, it will only show unstaged changes
        # https://stackoverflow.com/questions/13057457/show-both-staged-working-tree-in-git-diff
        # use `git diff --staged` to see only the staged changes
    elif [[ "$1" =~ '^\d+$' ]] ; then
        set -- "HEAD~$1"
    fi

    if false && isKitty ; then
        git-diff-kitty "$@"
    else
        git diff --submodule=diff "$@"
    fi
}
##
