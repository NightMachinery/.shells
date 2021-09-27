##
function irc-chatlog-rmself {
    cat-paste-if-tty | sd '^\s*('${(j.|.)irc_usernames}')\W*' ' ' | cat-copy-if-tty
}
##
function irc-chatlog-dir-get {
    local d="${znc_dir}"
    if test -z "$d" ; then
      if isServer ; then
        d=~/.znc/moddata
      else
        d="$(ffz-get chat_logs/znc)" @TRET
      fi
    fi
    d+="/log/${irc_usernames[1]}/${irc_networks[1]}/"
    test -d "$d" @TRET
    ec "$d"
}

function irc-ugb {
    local query="$*"
    local d
    d="$(irc-chatlog-dir-get)" @TRET

    indir-exists "$d" @opts follow y @ ugbase --hidden '--binary-files=without-match' --with-filename --line-number --bool -- '-"-SaslServ-" -"-NickServ-" -"*** Quits:" -"*** Joins:"  greyrat|lucerne '"$query"
}
@opts-setprefix irc-ugb irc_sees
##
function irc-sees {
    # local irc_sees_fz_query=("$@")

    ## @duplicateCode/829eb56b1afc0980f19e0730eb09699f
    local dirs
    dirs=("$(irc-chatlog-dir-get)") @TRET
    local dir_main="${dirs[1]}"
    local nightNotes="${dir_main}"
    ##

    @opts engine [ h-irc-nts-engine "$@" ]  @ ntsearch-lines
}

function h-irc-nts-engine {
    local fz_query
    fz_query="$(fzp_ug="${fzp_ug:-ni}" fz-createquery "${irc_sees_fz_query[@]}")" @TRET

    ## @duplicateCode/829eb56b1afc0980f19e0730eb09699f
    local dirs
    dirs=("$(irc-chatlog-dir-get)") @TRET
    local dir_main="${dirs[1]}"
    local nightNotes="${dir_main}"
    ##

    irc-ugb "$@" | revaldbg @opts dir_main "$dir_main" query "$fz_query" opts [ --no-sort ] @ h-grep-output-to-fz
}
##
