tldr() { 
	  #nig ea  #not needed because of piping autoremoval of color.
	  isDarwin && { command tldr "$@" || command tldr -p linux "$@" ; return $? }
	  command tldr "$@" | bt
}
exa() {
    local arg long=''
    for arg in "$@"
    do
        [[ "$arg" == "-l" || "$arg" == "--long" ]] && long='y'
    done
    if test -z "$long"
    then
        command exa -1 "$@"
    else
        command exa "$@"
    fi
}
