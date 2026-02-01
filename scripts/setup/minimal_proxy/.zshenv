###
#: NOTE We load [[nightNotesPublic:cheatsheets/OS/unix/tangled/lib.zsh]] in this file. (=~/base/bootstrap/lib.zsh=)
###
#: [[id:3c5fc2e7-2ed9-4b14-b1aa-11337439277a][{BUG} · Issue #331 · Aloxaf/fzf-tab]]
autoload -Uz zmathfunc
zmathfunc
###
setopt aliases
setopt interactivecomments
setopt NO_CASE_GLOB
setopt autocd multios re_match_pcre extendedglob pipefail interactivecomments hash_executables_only # hash_executables_only will not hash dirs instead of executables, but it can be slow.
setopt long_list_jobs complete_in_word always_to_end
setopt append_history extended_history hist_expire_dups_first hist_ignore_dups hist_ignore_space hist_verify inc_append_history share_history
setopt TYPESET_SILENT # Without this, the local/typeset commands display the value of any variable which is already defined.
unsetopt autopushd
unsetopt AUTO_NAME_DIRS
unsetopt BG_NICE # Run all background jobs at a lower priority.
# having this enabled will cause some failures in BTT-issued background brishz commands
## * Global Aliases
alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'
alias -g ......='../../../../..'

alias -g '@RET'=' || { retcode=$? ; print -r -- "Inside $0: exited ${retcode}" ; return $retcode } '
##
ec() {
    print -r -- "$@"
}
ecn() {
    print -nr -- "$@"
}
alias ecgray=ec

ecerr() {
    ec "$@" >&2
}

gquote () {
    ec "${(q+@)@[1]}" "${(qq@)@[2,-1]}"
}
##
alias var-show='>&2 typeset -p'
alias typ='var-show'

function var-get {
    ec "${(P)1}"
}
##
function export-from-alias {
  local name="${1:?}"

  local code="${aliases[$name]}"
  if test -n "$code" ; then
    eval export "${code}"
  fi
  ##
  # This is safe:
  #
  # ❯ alias hi='tmp="a ;b; c"'
  # ❯ ec ${aliases[hi]}
  # tmp="a ;b; c"
  ##
}

source ~/.shared.sh
psource ~/base/bootstrap/lib.zsh
###
export HISTFILE="${HOME}/.zsh_history"
export HISTSIZE=1000000
export SAVEHIST=1000000
###
function cron-commands-reboot-get {
    crontab -l |
        perl -nle 'print $1 if /^\s*\@reboot\s+(.+)/'
}

function cron-commands-reboot-run {
    local cmds
    cmds=(${(@f)"$(cron-commands-reboot-get)"})

    for cmd in $cmds[@] ; do
        echo "$cmd"
        eval "$cmd"
    done
}
##
alias rh=rehash
##
function wget-dir {
    # @alt dl-dir
    ##
    # https://stackoverflow.com/questions/17282915/how-to-download-an-entire-directory-and-subdirectories-using-wget
    ##
    local depth="${wget_dir_d:-0}" url="${@[-1]}" opts=()

    local parent_count
    if parent_count="$(url_dir_count.pl "$url")" ; then
        opts+=( --cut-dirs $((parent_count - 1)) )
    fi

    wget -e robots=off -r --level="$depth" --no-host-directories --no-parent --reject="index.html*" --no-clobber "${opts[@]}" "$@"
    # --level (-l) determines the depth of the recursion (5 by default, use 0 or 'inf' for unlimited):
    #
    # + -l1 just download the directory (tzivi in your case)
    #
    # + -l2 download the directory and all level 1 subfolders ('tzivi/something' but not 'tivizi/somthing/foo')
    #
    # --no-host-directories (-nH) option with wget to prevent the hostname directory getting created by default with the download directory.
    #
    # --cut-dirs=X (cuts out X directories)(Big X acts effectively like --no-directories)
    #
    # --no-directories (-nd): do not create a hierarchy of directories when retrieving recursively. With this option turned on, all files will get saved to the current directory, without clobbering
    #
    # --no-clobber: When running Wget with -r or -p, but without -N, -nd, or -nc, re-downloading a file will result in the new copy simply overwriting the old.  Adding -nc will prevent this behavior, instead causing the original version to be preserved and any newer copies on the server to be ignored.
    #
    # --timestamping (-N): uses the timestamps of the file and the server to determine if it has changed. (Presumably.)
    # When running Wget with -N, with or without -r or -p, the decision as to whether or not to download a newer copy of a file depends on the local and remote timestamp and size of the file.  -nc may not be specified at the same time as -N.
    ##
}
##
function uv-pip {
    if isDefined-cmd uv ; then
        if test -z "${CONDA_PREFIX}" ; then
            for d in ~/anaconda ~/miniforge3 ~/miniconda3 ; do
                if test -e "$d" ; then
                    local -x CONDA_PREFIX="$d"

                    break
                fi
            done
        fi
        # var-show CONDA_PREFIX

        $proxyenv command uv pip "$@"
    else
        pip "$@"
    fi
}

function pip-install {
    #: @duplicateCode/47062dbd66e1829f2778ed5684a5e8ff
    ##
    if [[ "$1" =~ '^git\+' ]] ; then
        #: =uv= doesn't support =git= links
        ##
        pip install "$@"
        pip install --no-deps --force-reinstall "$@"
    else
        uv-pip install -U "$@"
    fi
}
alias pi='\noglob pip-install'
##
function plg-log-last-reval() {
  local last_file=$(ls -t ~/.plg | head -1)
  if [[ -n $last_file ]]; then
    reval "$@" ~/.plg/$last_file
  else
    echo "No files found in ~/.plg directory"
  fi
}

function plg-log-last {
  plg-log-last-reval tail -f
}
##
function head-tail {
    local from="${1}" to="${2}"

    integer len=$((to - (from - 1)))
    head -n "${to}" | tail -n "${len}"
}

function sort-by-regex-perl {
    #: @duplicateCode/22b86ed758257c1f1036c9b3cdf93542
    #:
    #: @alt [[NIGHTDIR:rust/floatsort.rs][floatsort]]
    #:
    #: @example
    #: You can sort the lines by the 'n' number first, and if the numbers are the same, by the fruit name:
    #: echo -e 'apple 1 n:10\nbanana 2 n:20\ncherry 3 n:10\ndate 4 n:30' |
    #:   sort-by-regex-perl -a 'n:(\d+)' -d '(\w+)'
    ##
    if [[ $# -lt 1 ]]; then
        echo "Usage: sort-by-regex-perl [-a|--ascending] [-d|--descending] <pattern1> [<pattern2> ...] [--]"
        return 1
    fi

    perl -e '
    use strict;
    use warnings;
    use v5.34.0;
    use Getopt::Long;

    my @patterns;
    my @orders;
    my $default_order = "ascending";
    # my $default_order = "descending";
    my $current_order = $default_order;

    GetOptions(
        "a|ascending" => sub { $current_order = "ascending"; },
        "d|descending" => sub { $current_order = "descending"; },
        "<>" => sub {
            push @patterns, { pattern => $_[0], order => $current_order };
            $current_order = $default_order;
        }
    );

    if (@patterns == 0) {
        die "Usage: sort-by-regex-perl [-a|--ascending] [-d|--descending] <pattern1> [<pattern2> ...] [--]\n";
    }

    my @lines = <STDIN>;

    print sort {
        my $result = 0;
        for my $entry (@patterns) {
            my $pattern = $entry->{pattern};
            my $order = $entry->{order};
            my ($a_match) = $a =~ /$pattern/;
            my ($b_match) = $b =~ /$pattern/;
            $a_match //= "";
            $b_match //= "";

            # Use numeric comparison if both matches are numbers, otherwise use string comparison
            if ($a_match =~ /^-?\d+(?:\.\d+)?$/ && $b_match =~ /^-?\d+(?:\.\d+)?$/) {
                $result = $a_match <=> $b_match;
            } else {
                $result = $a_match cmp $b_match;
            }

            # print STDERR "Comparing $a_match (from pattern $pattern) and $b_match: $result\n";

            $result = -$result if $order eq "descending";
            last if $result != 0;
        }
        $result;
    } @lines;
    ' -- "$@"
}
##
alias sbb='exec ${commands[zsh]}'
#: We can't use our own 'sbb' here. When we remove all the env vars, the terminal breaks. I guess this has to do with the terminal being remote? Anyhow, I don't know which exact env var is needed to stop the breakage.
##
#: @duplicateCode/fd8ad3a5d0229646fb38ce64e1cede19
function aider-m {
    local model="${aider_model}"
    local opts=(
        --map-tokens=0
        #: Max number of tokens to use for repo map, use 0 to  disable (default: 1024) [env var: AIDER_MAP_TOKENS]
    )

    if git-clean-p ; then
        # The repository is clean

        local -x OPENROUTER_API_KEY="${openrouter_api_key}"
        local -x OPENAI_API_KEY="${openai_api_key}"
        #: Some models below replace OPENAI_API_KEY.
        #: This is bad, as aider uses OpenAI for its voice mode, too.

        if [[ "$model" == c3o ]] ; then
            opts+=(
                --model
                openrouter/anthropic/claude-3-opus:beta

                --edit-format diff
            )
            ##
            # local -x OPENAI_API_KEY="${openrouter_api_key}"
            # local -x OPENAI_API_BASE=https://openrouter.ai/api/v1

            # opts+=(
            #     --model
            #     # anthropic/claude-3-opus
            #     anthropic/claude-3-opus:beta

            #     --edit-format diff
            # )
            ##

        elif [[ "$model" == s3 ]] ; then
            opts+=(
                --model
                # openrouter/anthropic/claude-3.5-sonnet:beta
                #: The beta variant is not recognized by aider.
                openrouter/anthropic/claude-3.5-sonnet

                --edit-format diff
            )

        elif [[ "$model" == g1.5 ]] ; then
            local -x OPENAI_API_KEY="${openrouter_api_key}"
            local -x OPENAI_API_BASE=https://openrouter.ai/api/v1

            opts+=(
                --model
                google/gemini-pro-1.5

                # --edit-format diff
            )

        elif [[ "$model" == gq-llama3 ]] ; then
            local -x GROQ_API_KEY="${groq_api_key}"

            opts+=(
                --model
                groq/llama3-70b-8192

                # --edit-format diff
            )
        fi

        $proxyenv reval-ecgray command aider "${opts[@]}" "$@"
    else
        ecerr "$0: Repository is dirty"
        return 1
    fi
}
aliasfn aider aider-m
aliasfn aider-4 aider_model=gpt-4 aider-m
aliasfn aider-4t aider_model=gpt-4-turbo aider-m
aliasfn aider-s3 aider_model=s3 aider-m
aliasfn aider-c3o aider_model=c3o aider-m
aliasfn aider-g1.5 aider_model=g1.5 aider-m
aliasfn aider-l3 aider_model=gq-llama3 aider-m
##
function decompv-run {
    (
        # typeset -ag exit_traps=( 0 INT TERM HUP EXIT )
        # setopt localtraps ; trap "" $exit_traps[@]

        local log_file="${HOME}/logs/run_$EPOCHSECONDS"
        mkdir -p "${log_file:h}"

        ec "log_file: ${log_file}"

        python ~/code/DecompV/decompv/x/run/run_v1.py "$@" |& tee "${log_file}"
        #: `setsid --wait --fork --` does not seem to work for making the interrupt signal go to the Python script.
        #: Use `q` to exit pdb instead of using ctrl-c, problem solved, no need for complex signal handling.

        # trap - $exit_traps[@]
    )
}
##
function pbcopy-file-as-md {
    #: @duplicateCode/2f9a53358e9a32d7c5642a4a9842dd63
    ##
    local fs=($@)

    local f retcode=0
    for f in "${fs[@]}" ; do
        if ! test -e "$f" ; then
            ecerr "$0: File does not exist: $f"
            retcode=1
            continue
        fi

        ec "File: $f"
        ec '``````````'
        cat "$f"
        ec '``````````'
        ec $'\n'
    done | {
        if isLocal ; then
            cat-copy-streaming
        else
            pbcopy-remote
        fi
    }

    return $retcode
}
alias cx='pbcopy-file-as-md'
### * End
psource ~/.private.env.zsh
psource ~/.privateShell
###
