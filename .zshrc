# @perf zshrc takes a lot of time (2s?). Most of these seem to be from compinit. Hard to profile these as their second runs are lighter.
###
function nightsh-load-zshrc() {
  bicon_force_plugins=y
  ###
  terminfo-set-auto
  ### OMZ config (no longer loaded)
  DEFAULT_USER="evar"
  # If you come from bash you might have to change your $PATH.
  # export PATH=$HOME/bin:/usr/local/bin:$PATH

  # Set name of the theme to load. Optionally, if you set this to "random"
  # it'll load a random theme each time that oh-my-zsh is loaded.
  # See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
  # ZSH_THEME="robbyrussell"
  # ZSH_THEME="oppa-lana-style"
  # ZSH_THEME="powerlevel9k/powerlevel9k"

  # Set list of themes to load
  # Setting this variable when ZSH_THEME=random
  # cause zsh load theme from this variable instead of
  # looking in ~/.oh-my-zsh/themes/
  # An empty array have no effect
  # ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

  # Uncomment the following line to use case-sensitive completion.
  # CASE_SENSITIVE="true"

  # Uncomment the following line to use hyphen-insensitive completion. Case
  # sensitive completion must be off. _ and - will be interchangeable.
  # HYPHEN_INSENSITIVE="true"

  # Uncomment the following line to disable bi-weekly auto-update checks.
  # DISABLE_AUTO_UPDATE="true"

  # Uncomment the following line to change how often to auto-update (in days).
  # export UPDATE_ZSH_DAYS=13

  # Uncomment the following line to disable colors in ls.
  # DISABLE_LS_COLORS="true"

  # Uncomment the following line to disable auto-setting terminal title.
  # DISABLE_AUTO_TITLE="true"

  # Uncomment the following line to enable command auto-correction.
  # ENABLE_CORRECTION="true"

  # Uncomment the following line to display red dots whilst waiting for completion.
  # COMPLETION_WAITING_DOTS="true"

  # Uncomment the following line if you want to disable marking untracked files
  # under VCS as dirty. This makes repository status check for large repositories
  # much, much faster.
  # DISABLE_UNTRACKED_FILES_DIRTY="true"

  # Uncomment the following line if you want to change the command execution time
  # stamp shown in the history command output.
  # The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
  # HIST_STAMPS="mm/dd/yyyy"

  # Would you like to use another custom folder than $ZSH/custom?
  # ZSH_CUSTOM=/path/to/new-custom-folder

  # Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
  # Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
  # Example format: plugins=(rails git textmate ruby lighthouse)
  # Add wisely, as too many plugins slow down shell startup.
  # plugins=(git git-extras lein pip sbt scala screen sprunge sudo vi-mode redis-cli)
  plugins=(git git-extras)
  isDarwin && plugins+=(osx)
  # export ZSH=~/.oh-my-zsh
  # source $ZSH/oh-my-zsh.sh # @retiredperf this takes ~2.5s

  # User configuration

  # export MANPATH="/usr/local/man:$MANPATH"

  # You may need to manually set your language environment
  # export LANG=en_US.UTF-8

  # Preferred editor for local and remote sessions
  # if [[ -n $SSH_CONNECTION ]]; then
  #   export EDITOR='vim'
  # else
  #   export EDITOR='mvim'
  # fi

  # Compilation flags
  # export ARCHFLAGS="-arch x86_64"

  # ssh
  # export SSH_KEY_PATH="~/.ssh/rsa_id"

  # Set personal aliases, overriding those provided by oh-my-zsh libs,
  # plugins, and themes. Aliases can be placed here, though oh-my-zsh
  # users are encouraged to define aliases within the ZSH_CUSTOM folder.
  # For a full list of active aliases, run `alias`.
  #
  # Example aliases
  # alias zshconfig="mate ~/.zshrc"
  # alias ohmyzsh="mate ~/.oh-my-zsh"
  ###

  zsh-defer psource /usr/local/opt/git-extras/share/git-extras/git-extras-completion.zsh

  ## vi-mode (forked from OMZ)
  # Updates editor information when the keymap changes.
  function zle-keymap-select() {
    zle reset-prompt
    zle -R
  }

  # Ensure that the prompt is redrawn when the terminal size changes.
  TRAPWINCH() {
    zle &&  zle -R
  }

  zle -N zle-keymap-select
  zle -N edit-command-line


  ### vim
  if true ; then
    # if false ; then
    set -o vi
    bindkey -v

    # allow v to edit the command line (standard behaviour)(does not work for @me? It opens emacs ...)
    autoload -Uz edit-command-line
    bindkey -M vicmd 'v' edit-command-line
  else
    # usable but noticably slow
    # https://github.com/jeffreytse/zsh-vi-mode
    export ZVM_LAZY_KEYBINDINGS=false
    source-plugin jeffreytse/zsh-vi-mode
    ZVM_LAZY_KEYBINDINGS=false
    ZVM_LINE_INIT_MODE=$ZVM_MODE_INSERT
    ZVM_VI_HIGHLIGHT_BACKGROUND='#c2e4ff'
    # ZVM_INSERT_MODE_CURSOR=$ZVM_CURSOR_BEAM
    # ZVM_NORMAL_MODE_CURSOR=$ZVM_CURSOR_BLOCK
    ZVM_INSERT_MODE_CURSOR=$ZVM_CURSOR_USER_DEFAULT
    # ZVM_NORMAL_MODE_CURSOR=$ZVM_CURSOR_BLINKING_BLOCK # doesn't blink
    ZVM_NORMAL_MODE_CURSOR=$ZVM_CURSOR_UNDERLINE
    ZVM_OPPEND_MODE_CURSOR=$ZVM_CURSOR_UNDERLINE
    # config needed:
    # * need to use hooks to load ctrl-r (fzf) and other conflicts https://github.com/jeffreytse/zsh-vi-mode#execute-extra-commands
  fi
  ###
  # allow ctrl-p, ctrl-n for navigate history (standard behaviour)
  bindkey '^P' up-history
  bindkey '^N' down-history

  # allow ctrl-h, ctrl-w, ctrl-? for char and word deletion (standard behaviour)
  bindkey '^?' backward-delete-char
  bindkey '^h' backward-delete-char
  bindkey '^w' backward-kill-word

  # allow ctrl-a and ctrl-e to move to beginning/end of line
  bindkey '^a' beginning-of-line
  bindkey '^e' end-of-line

  # if mode indicator wasn't setup by theme, define default
  if [[ "$MODE_INDICATOR" == "" ]]; then
    MODE_INDICATOR="%{$fg_bold[red]%}<%{$fg[red]%}<<%{$reset_color%}"
  fi

  function vi_mode_prompt_info() {
    echo "${${KEYMAP/vicmd/$MODE_INDICATOR}/(main|viins)/}"
  }

  # define right prompt, if it wasn't defined by a theme
  if [[ "$RPS1" == "" && "$RPROMPT" == "" ]]; then
    RPS1='$(vi_mode_prompt_info)'
  fi
  ##
  #This doesn't work. I have no idea why.
  bindkey -M viins ‘ii’ vi-cmd-mode
  bindkey -v

  bindkey '^[^M' self-insert-unmeta # You can use self-insert-unmeta to bind Alt+Return to insert a literal newline without accepting the command

  #: self-insert-unmeta causes ={=, =}= to not be escaped when typed in the middle of, e.g., a URL. I don't understand why this happens. My guess is that one of my plugins is hooking into =self-insert= to insert the escape, but =self-insert-unmeta= is not hooked.
  #: self-insert-unmeta: Insert a character into the buffer after stripping the meta bit and  converting ^M to ^J.
  bindkey '\{' self-insert-unmeta
  bindkey '\}' self-insert-unmeta
  ##
  # start typing + [Up-Arrow] - fuzzy find history forward
  if [[ "${terminfo[kcuu1]}" != "" ]]; then
    autoload -U up-line-or-beginning-search
    zle -N up-line-or-beginning-search
    bindkey "${terminfo[kcuu1]}" up-line-or-beginning-search
    bindkey "^[[A" up-line-or-beginning-search
  fi
  # start typing + [Down-Arrow] - fuzzy find history backward
  if [[ "${terminfo[kcud1]}" != "" ]]; then
    autoload -U down-line-or-beginning-search
    zle -N down-line-or-beginning-search
    bindkey "${terminfo[kcud1]}" down-line-or-beginning-search
    bindkey "^[[B" down-line-or-beginning-search
  fi
  bindkey '^[[1;3D' backward-word # alt-left
  bindkey '^[[1;3C' forward-word # alt-right
  bindkey '^[/' forward-word # alt-/
  ### iTerm Integration
  alias it2attention=~/.iterm2/it2attention
  alias it2check=~/.iterm2/it2check
  alias it2copy=~/.iterm2/it2copy
  alias it2dl=~/.iterm2/it2dl
  alias it2getvar=~/.iterm2/it2getvar
  alias it2setcolor=~/.iterm2/it2setcolor
  alias it2setkeylabel=~/.iterm2/it2setkeylabel
  alias it2ul=~/.iterm2/it2ul
  alias it2universion=~/.iterm2/it2universion
  ###
  function zle-enable-url-quote {
    # @warn this also quotes if you're pasting in quotes, which obviously ruins the result
    autoload -Uz url-quote-magic
    zle -N self-insert url-quote-magic
    autoload -Uz bracketed-paste-magic
    zle -N bracketed-paste bracketed-paste-magic
  }
  zle-enable-url-quote # @warn needs to be before stuff like syntax highlighters
  ##
  source-plugin mafredri/zsh-async
  zsh-defer source-plugin zdharma-continuum/zui

  # re source "$NIGHTDIR"/zshlang/personal/aliases.zsh "$NIGHTDIR"/bash/auto-load/aliases.bash #To make them have priority. # Sth makes zsh reload all aliases, which breaks `ialiases`.

  # autoload -U deer
  # zle -N deer
  # bindkey '\ek' deer

  if isExpensive ; then
    unsetopt correct_all

    fpath=(~/.zsh.d/ $fpath[@])

    autoload -Uz compinit && compinit # @heavy >0.35s
    autoload -U +X bashcompinit && bashcompinit
    zmodload -i zsh/complist
    _comp_options+=(globdots)
  fi

  source-interactive-all

  ## fzf zsh integration (obsolete, as we now use fzf-tab
  # # sth in .zshrc overrides these so ...
  # export FZF_BASE="${$(ge_no_ec=y realpath2 fzf):h:h}/shell"
  # # ecdbg "FZF_BASE: $FZF_BASE"
  # test -d "$FZF_BASE" && {
  #     # ecdbg fzf loaded
  #     re source "$FZF_BASE"/*.zsh
  #     export FZF_COMPLETION_TRIGGER=''
  #     bindkey '^T' fzf-completion
  #     bindkey '^I' expand-or-complete
  #     bindkey '^[[Z' fzf-completion #Shift+Tab
  # }
  ##
  function fzf-history-widget() {
    # copied from fzf zsh integration
    local selected num
    setopt localoptions noglobsubst noposixbuiltins pipefail no_aliases 2> /dev/null

    local query
    # query="${(qqq)LBUFFER}"
    query="${LBUFFER}"
    query="$(fz-createquery ${=query})"
    # note that if we want to use C-r to find sth, we can activate from the start and start typing, but if we have already written a command and want to check it in C-r, quoting is necessary
    #
    # @tradeoff --no-sort

    selected=( $(fc -rl 1 | perl -ne 'print if !$seen{($_ =~ s/^\s*[0-9]+\s+//r)}++' |
                   FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-70%} $FZF_DEFAULT_OPTS -n2..,.. --no-sort --tiebreak=index --bind=ctrl-r:toggle-sort $FZF_CTRL_R_OPTS +m" fz --query="$query") )
    local ret=$?
    if [ -n "$selected" ]; then
      num=$selected[1]
      if [ -n "$num" ]; then
        zle vi-fetch-history -n $num
      fi
    fi
    zle reset-prompt
    return $ret
  }
  zle     -N   fzf-history-widget

  bindkey '^R' fzf-history-widget # C-r
  ##
  # allow ctrl-r to perform backward search in history
  # bindkey '^r' history-incremental-search-backward
  #

  zle -N fr_zle
  bindkey '^[[Z' fr_zle_deus # shift+tab
  zle -N fr_zle_deus
  bindkey '^[\t' fr_zle # alt+tab

  zle -N clipboard-fz-widget
  bindkey '^p' clipboard-fz-widget
  ##

  # zsh-defer psource ~/Library/Preferences/org.dystroy.broot/launcher/bash/br
  ##
  # source-plugin intelfx/pure
  source-plugin sindresorhus/pure

  bella_zsh_disable1=''
  function prompt_pure_check_cmd_exec_time () {
    # Obviously, we are abusing pure's private API here :D We can achieve the same using the preexec and precmd hooks, but reusing this seems cheaper
    integer elapsed
    (( elapsed = EPOCHSECONDS - ${prompt_pure_cmd_timestamp:-$EPOCHSECONDS} ))
    typeset -g prompt_pure_cmd_exec_time=
    (( elapsed > ${PURE_CMD_MAX_EXEC_TIME:-5} )) && {
      prompt_pure_human_time_to_var $elapsed "prompt_pure_cmd_exec_time"
    }

    if (( elapsed > ${BELL_EXEC_TIME:-5} )) ; then
      bella-zsh-gateway
    fi
    bella_zsh_disable1=''
    ##
  }
  chpwd_functions=(${chpwd_functions[@]} zsh-pwd-save)
  if bool "$ZDIRS_ENABLED" ; then
    chpwd_functions+="z-add-pwd"
  fi
  ##
  # source-plugin denysdovhan/spaceship-prompt
  # source-plugin Tarrasch/zsh-bd
  # source-plugin Tarrasch/zsh-colors
  # source-plugin Vifon/deer
  zsh-defer source-plugin unixorn/git-extra-commands

  ##
  # zsh-defer source-plugin zdharma-continuum/zzcomplete # binds C-f, ^F
  # does not support light themes https://github.com/zdharma/zzcomplete/issues/1
  ##

  if test -n "$bicon_force_plugins" || ! isBicon ; then
    ##
    export ZSH_AUTOSUGGEST_USE_ASYNC=y # idk if export is necessary
    zsh-defer source-plugin zsh-users/zsh-autosuggestions

    typeset -g ZSH_AUTOSUGGEST_STRATEGY
    ZSH_AUTOSUGGEST_STRATEGY=(history completion)
    ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=200 # default is unlimited

    if isKitty ; then
      ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#000000"
    else
      # ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#ff00ff,bg=cyan,bold,underline"
    fi
    ##
  fi
  silent unalias =
  # source-plugin zsh-users/zsh-syntax-highlighting
  source-plugin zsh-users/zsh-completions #undeferable
  ##
  YSU_MESSAGE_POSITION="after"
  YSU_MESSAGE_FORMAT="$(tput setaf 1)found %alias_type for %command: %alias$(tput sgr0)"
  YSU_MODE=ALL #BESTMATCH or ALL
  YSU_IGNORED_ALIASES=("g" "ll" "o")
  source-plugin "MichaelAquilina/zsh-you-should-use"

  function _check_aliasfns() { # @dep/private
    local typed="$1"
    local expanded="$2"

    local found_aliases
    found_aliases=()
    local best_match=""
    local best_match_value=""
    local key
    local value

    # Find alias matches
    for key in "${(@k)aliases_fn}"; do
        value="${aliases_fn[$key]}"

        # Skip ignored aliases
        if [[ ${YSU_IGNORED_ALIASES[(r)$key]} == "$key" ]]; then
            continue
        fi

        if [[ "$typed" = "$value" || \
              "$typed" = "$value "* ]]; then

        # if the alias longer or the same length as its command
        # we assume that it is there to cater for typos.
        # If not, then the alias would not save any time
        # for the user and so doesn't hold much value anyway
        if [[ "${#value}" -gt "${#key}" ]]; then
            found_aliases+="$key"
        fi
        fi
    done

    # Print result matches based on current mode
    for key in ${(@ok)found_aliases}; do
      value="${aliases_fn[$key]}"
      ysu_message "alias_fn" "$value" "$key"
    done

    if [[ -n "$found_aliases" ]]; then
        _check_ysu_hardcore
    fi
  }

  add-zsh-hook preexec _check_aliasfns
  ##

  ##
  # source-plugin djui/alias-tips # incompatible with our own alias module
  # export ZSH_PLUGINS_ALIAS_TIPS_EXCLUDES="_ ll vi"
  # export ZSH_PLUGINS_ALIAS_TIPS_REVEAL_EXCLUDES=(_ ll vi)
  # alias revealaliases='export ZSH_PLUGINS_ALIAS_TIPS_REVEAL=1'
  ##

  ###
  function zle-print-dots() {
    # toggle line-wrapping off and back on again
    [[ -n "$terminfo[rmam]" && -n "$terminfo[smam]" ]] && echoti rmam
    # print -Pn "%{%F{red}......%f%}"
    print -Pn "%{%F{green}...%f%}"
    [[ -n "$terminfo[rmam]" && -n "$terminfo[smam]" ]] && echoti smam
  }
  function zle-complete-with-dots() {
    zle-print-dots
    ##
    # This expands variables and globs if possible, else it auto-completes:
    # zle expand-or-complete
    ##
    # zle fzf-tab-complete # causes infinite loop
    ##
    zle complete-word
    ##
    zle redisplay
  }
  zle -N zle-complete-with-dots
  bindkey "^I" zle-complete-with-dots # TAB

  function zle-expand() {
    zle expand-word
    # zle redisplay # @bug does not syntax-color
  }
  zle -N zle-expand
  bindkey "^_" zle-expand # C-/
  ##
  FZTAB_OPTS=(
    --color=light # needed, as fzf-tab overrides the colors, so we need to re-override them
    -i # case-insensitive (as opposed to smart-case)
    # --height='${FZF_TMUX_HEIGHT:=75%}'
    # --sync # did not solve the weird fzf bug
  )
  function fztab {
    ##
    local query opts=()
    for arg in "$@" ; do
        if [[ "$arg" =~ '^(?:--query|-q)=(.*)' ]] ; then
          query="${match[1]}"
          query="$(fz-createquery "$query")" @TRET

          opts+="--query=${query}"
        else
          opts+="$arg"
        fi
    done

    command fzf "${opts[@]}" "$FZTAB_OPTS[@]"

    # fnswap isI true ugfz "$@" "$FZTAB_OPTS[@]" ""
    ##
    # Do NOT use --exit-0 or in macOS you will not be able to start typing before the fzf window opens
    # fzf_mru_context= fzf-gateway  "$@" "$FZTAB_OPTS[@]"

    # Check out /Users/evar/Library/Caches/antibody/https-COLON--SLASH--SLASH-github.com-SLASH-Aloxaf-SLASH-fzf-tab/lib/-ftb-fzf to see how this is run
  }
  # zstyle ':fzf-tab:*' print-query alt-enter # this is the default
  zstyle ':fzf-tab:*' fzf-command fztab
  # zstyle ':fzf-tab:*' fzf-command fzf
  zstyle ':completion:*' format '[%d]' # enables groups
  zstyle ':fzf-tab:*' switch-group ',' '.'
  zstyle ':fzf-tab:*' prefix ''
  zstyle ':fzf-tab:*' default-color $'\033[38;5;24m'
  ##
  FZF_TAB_GROUP_COLORS=(
    $'\033[38;5;24m' $'\033[32m' $'\033[33m' $'\033[35m' $'\033[31m' $'\033[38;5;27m' $'\033[36m' \
      $'\033[38;5;100m' $'\033[38;5;98m' $'\033[91m' $'\033[38;5;80m' $'\033[92m' \
      $'\033[38;5;214m' $'\033[38;5;165m' $'\033[38;5;124m' $'\033[38;5;120m'
  )
  # FZF_TAB_GROUP_COLORS=()
  # for i in {1..16} ; do
  #     # FZF_TAB_GROUP_COLORS+="$(colorfg 10 255 10)"
  #     FZF_TAB_GROUP_COLORS+=$'\033[38;5;19m'
  # done
  zstyle ':fzf-tab:*' group-colors $FZF_TAB_GROUP_COLORS
  ##

  source-plugin Aloxaf/fzf-tab # should come after all tab keybindings
  ## fzf-tab has abandoned this API
  # fzf-tab-partial-and-complete() {  # @overrides
  #     if [[ $LASTWIDGET = 'fzf-tab-partial-and-complete' ]]; then
  #         fzf-tab-complete
  #     else
  #         zle complete-word
  #     fi
  # }
  # # zle -N fzf-tab-partial-and-complete
  # bindkey '^I' fzf-tab-partial-and-complete
  ##
  fzf_tab_complete_code="${functions[fzf-tab-complete]}"
  fzf-tab-complete() { # @overrides
    per2en-buffer
    if [[ "$out_sym2983387" == per ]] ; then
      input-lang-set en
    fi
    eval "$fzf_tab_complete_code"
  }
  ##
  zstyle -d ':completion:*' special-dirs # to not show ./ and ../
  # https://unix.stackexchange.com/a/14231/282382
  # to make tab work on empty line
  zstyle ':completion:*' insert-tab false
  ##
  # zstyle ':completion:*' matcher-list '+m:{a-zA-Z}={A-Za-z}' '+r:|[._-]=* r:|=*' 'r:|?=** m:{a-z\-}={A-Z\_}' '+l:|=* r:|=*'
  zstyle ':completion:*' matcher-list 'r:|?=** m:{a-z\-}={A-Z\_}' '+l:|=* r:|=*'

  # use `zstyle ':completion:*' matcher-list '' '+m:{a-zA-Z}={A-Za-z}' '+r:|[._-]=* r:|=*' '+l:|=* r:|=*'` if you don't want to see the case-insesitive results if there are case-sensitive results. In general, it seems that the matches stop at the first rule that produces at least one match.

  # See:
  # https://superuser.com/questions/415650/does-a-fuzzy-matching-mode-exist-for-the-zsh-shell
  # http://zsh.sourceforge.net/Doc/Release/Completion-System.html
  # http://zsh.sourceforge.net/Doc/Release/Completion-Widgets.html#Completion-Matching-Control
  # These are general rules that make the completion system match better:
  # '+m:{a-zA-Z}={A-Za-z}' adds case-insensitivity.
  # '+r:|[._-]=* r:|=*' allows for partial completion before ., _ or -, e.g. f.b -> foo.bar.
  # '+l:|=* r:|=*' allows for completing on the left side of the written text, e.g. bar -> foobar)
  # 'r:|?=** m:{a-z\-}={A-Z\_}' full flex completion (abc => ABraCadabra)
  ##
  # (( $+commands[cod] )) && source <(command cod init $$ zsh | sd '\bcod\b' 'command cod')
  # https://github.com/dim-an/cod/issues/24
  ###
  if test -n "$bicon_force_plugins" || ! isBicon ; then
    zsh-defer source-plugin zdharma-continuum/fast-syntax-highlighting #: should be last
  fi
  # zsh-defer source-plugin zdharma-continuum/zbrowse # ^b # should be after fast-syntax, idk why but errors out otherwise
  ##
  # DISABLE_AUTO_TITLE="true" # disables omz setting the title
  omz_termsupport_precmd () {
    # @omz adds this to precmd_functions
    if [[ "$DISABLE_AUTO_TITLE" == true ]]
    then
      return
    fi
    tty-title "${PWD:t}"
  }
  precmd_functions+=(omz_termsupport_precmd)
  prompt_pure_set_title() true # disables pure setting the title
  ###
  # https://stackoverflow.com/a/14634437/1410221
  function per2en-buffer() {
    out_sym2983387=en
    ## perf
    # `BUFFER="ahjaha ah  isi s s" fnswap zle true time2 cmd-modifier`
    # `BUFFER="زjhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhjhhibjhiuutybyiyyhbbhgdryahjaha ah  isi s s" fnswap zle true time2 cmd-modifier`
    ##
    if [[ "$persian_exc_chars" == *"${BUFFER[1]:-A}"* ]] ; then
      BUFFER="$(ecn "$BUFFER" | per2en)"
      out_sym2983387=per
    fi
  }
  ##
  function cmd-modifier-rtl() {
    # @bug pollutes history
    # @warn `sbb | rtl...` redirects the stdout so you'll lose your normal stdout
    # @warn the piping forks the process and so you can no longer affect the main shell's state.
    if ! [[ "${BUFFER}" =~ ';\s*$' || "${BUFFER}" == *(rtl-off|rtl_reshaper_rs)* ]] ; then
      BUFFER+=" | rtl_reshaper_rs"
    fi
  }
  function rtl-on() {
    CM_RTL_MODE=y
  }
  function rtl-off() {
    CM_RTL_MODE=''
  }
  ##
  function cmd-modifier {
    per2en-buffer
    test -n "$CM_RTL_MODE" && cmd-modifier-rtl
    zle accept-line
  }
  zle -N cmd-modifier-widget  cmd-modifier
  function cmd-modifier-on {
    bindkey '^J' cmd-modifier-widget
    bindkey '^M' cmd-modifier-widget
  }
  function cmd-modifier-off {
    bindkey '^J' accept-line
    bindkey '^M' accept-line
  }
  cmd-modifier-on
  ZSH_AUTOSUGGEST_CLEAR_WIDGETS+=cmd-modifier-widget
  ###
  ## https://github.com/zsh-users/zsh-autosuggestions/issues/351
  # This speeds up pasting w/ autosuggest
  # https://github.com/zsh-users/zsh-autosuggestions/issues/238
  pasteinit() {
    OLD_SELF_INSERT=${${(s.:.)widgets[self-insert]}[2,3]}
    zle -N self-insert url-quote-magic # I wonder if you'd need `.url-quote-magic`?
  }

  pastefinish() {
    zle -N self-insert $OLD_SELF_INSERT
  }
  zstyle :bracketed-paste-magic paste-init pasteinit
  zstyle :bracketed-paste-magic paste-finish pastefinish

  # https://github.com/zsh-users/zsh-autosuggestions/issues/351
  ZSH_AUTOSUGGEST_CLEAR_WIDGETS+=(bracketed-paste)
  ###
  # remove '/' from word chars. Affects C-w, alt-left, alt-right, etc
  WORDCHARS="${WORDCHARS/\//}|'"
  WORDCHARS="${WORDCHARS/\#/}"
  WORDCHARS="${WORDCHARS/\./}"
  WORDCHARS="${WORDCHARS/\-/}"
  # WORDCHARS="${WORDCHARS/\|/}"
  WORDCHARS+='ضصثقفغعهخحجچشسیبلاتنمکگظطزرذدپو۱۲۳۴۵۶۷۸۹۰'

  function backward-kill-word2() {
    if [[ "$LBUFFER" =~ '(.*)\s+$' ]] ; then
      LBUFFER="$match[1]"
    else
      ## if you want to customize WORDCHARS per function:
      local WORDCHARS="${WORDCHARS}"
      zle backward-kill-word
    fi
  }
  zle -N backward-kill-word2
  bindkey '^W' backward-kill-word2 # C-w
  bindkey '^[^?' backward-kill-word2 # alt-backspace alt-delete
  ###
  # psource $HOME/.shellfishrc
  ##
  if isGrayfur ; then
    # somehow, we need to do this to make the PATH ordering correct
    source ~/.shared.sh
  fi
  ##
  # psource ~/.xsh # @futureCron remove me if unused
  ###
  rcLoaded='loading' # Do NOT export this, or `exec zsh` will inherit it
  zsh-defer typeset -g rcLoaded='yes'

  # tty-statusbar-enable
  isdefined bell-zsh-start && inbg bell-zsh-start || true
}
###
if nightsh-basic-p || { test -e "$nightsh_private_first" && ! source "$nightsh_private_first" } ; then
        function nsh() {
            nightsh-load-zshenv
            nightsh-load-bash
            nightsh-load-zshrc
            unfunction "$0"
        }
else
    nightsh-load-zshrc
fi
