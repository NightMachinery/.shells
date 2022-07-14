##
function rtl-reshaper-py {
  if isRtl ; then
      cat
  else
      rtl_reshaper.py "$@"
  fi
}

function rtl-reshaper {
  local wrap_p="${rtl_reshaper_wrap_p:-y}"

  if isRtl ; then
    cat
  else
    local -x COLUMNS="$COLUMNS"

    if bool "$wrap_p" ; then
      #: Very slow (because of ansifold)
      rtl_reshaper.dash "$@"
    else
      fribidi --nobreak
    fi
  fi
}

function rtl-reshaper-fast {
  rtl_reshaper_wrap_p=n rtl-reshaper "$@"
}

function reval-rtl {
  if isRtl ; then
    # not having a pipe can make more stuff work, so this opens the possibility of you seeing bugs later (i.e., some bugs will only become visible when you use rtl-reshaper)
    # but still, seeing bugs later is also a blessing, right? ;)
    reval "$@"
  else
    reval "$@" | rtl-reshaper
  fi
}
aliasfn rtl reval-rtl
##
function fz-rtl {
    fz "$@" --preview "$FZF_RTL_PREVIEW"
}

function fz-rtl1 {
    #: @warn The reshaped Persian text seemingly uses special chars so we can no longer search it using our normal Persian keyboard.
    #:
    #: https://github.com/junegunn/fzf/issues/2875
    ##
    local input="$(cat)" q="$1" opts=("${@[2,-1]}")

    local sels_i="$(ec "$input" | rtl-reshaper | cat -n | fz --with-nth 2.. --query "$q" "$opts[@]" | gawk '{print $1}')"
    gawk 'NR == FNR {nums[$1]; next} FNR in nums' <(ec "$sels_i") <(ec "$input")
}
##
function biconm {
  BICON_MODE=y bicon.bin "$@"
}

function bicon-emc {
  bella_zsh_disable1

  if isBicon ; then
  if true ; then
    emcnw "$@"
    return $?
  else
    ecerr "You're already in Bicon mode. Editing RTL text will be 'reversed'. Use a clean session instead."
    {
      (
        emacsclient -e "(progn (setq-default bidi-display-reordering nil) (redraw-display))"
        emc-gateway "$@"
      )
    } always {
      emacsclient -e "(setq-default bidi-display-reordering t)"
    }
    return $?
  fi
  fi

  if isKitty ; then
    emcnw "$@"
    return $?
  fi

  if isRtl || isKitty ; then
    # Kitty shapes the Persian letters itself, and Emacs handles its own BiDi reordering, so no need for bicon
    emc-gateway "$@"
  else
    $proxyenv biconm --reshape-only emacsclient -t "$@"
    reset
  fi
}
##
redis-defvar bicon_zsh_disabled
# Use bicon_zsh_disabled_del to enable bicon mode by default
function bicon-zsh {
  biconm zsh
  reset
}
##
function erase-bicon {
  gtr -d '\000'
}
##
