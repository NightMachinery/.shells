##
function rename-pipe {
    : "@alt rename-rec"
    : "to improve performance, filter the input to make it small"
    : '`a="_cropped-000" ; fd $a | rename-pipe sd $a ""`'

    : "@toRefactor rename-pipe-start | ... | rename-pipe-end"

    local files cmd=("$@") dry_run="${rename_pipe_dry:-${rename_pipe_n}}"
    files=(${(@f)"$(cat)"}) @TRET

    if bool $dry_run ; then
        ecgray "$0: dry run mode"
    fi

    local f new_path
    for f in $files[@] ; do
        new_path="$(ec "$f" | reval "$cmd[@]")" @TRET

        if [[ "$new_path" != "$f" ]] ; then
            if test -e "$f" ; then
                if bool $dry_run ; then
                    ecalternate "$f" "$new_path" @TRET
                else
                    ensure-dir "$new_path" @TRET
                    reval-ec gmv -i -v "$f" "$new_path" </dev/tty @TRET
                fi
            else
                ecbold "Path no longer exists: $f"
            fi
        fi
    done
}

aliasfn rename-pipe-dryrun rename_pipe_dry=y rename-pipe
@opts-setprefix rename-pipe-dryrun rename-pipe
##
function rename-rec {
    : "@alt rename-pipe"

    local max_depth="${rename_rec_max_depth:-100}"

    local d
    for d in {${max_depth}..1} ; do
        assert rename-depth "$d" "$@" @RET
    done

    ## tests:
    # `rename-rec '(.*)evil([^/]*)' '{:1}happy{:2}'`
    ##
}

function rename-depth() {
    local depth="${1}" dir="${rename_depth_dir:-.}" undo="${rename_depth_undo:-undo/undo}"
    assert-args depth dir undo @RET

    undo="${undo}_${depth}.json"
    assert ensure-dir "$undo" @RET

    nomino --generate "$undo" --depth "$depth" --dir "$dir" --print --mkdir "${@[2,-1]}"

    # --regex : idk what this does really
    # --test : dry-run
}
##
function mv-date() {
    local f="$1"
    assert test -e "$f" @RET

    local ext="$(ecn $f | rget '(\.[^.]*$)')"

    gmv -v -i "$f" "${f:r}_$(gdate +"%Y-%b-%d")${ext}"
}
reify mv-date
##
function rename-audio-auto {
  local input_file="$1"
  local format="${input_file:e}"
  assert-args input_file @RET

  title="$(ffmpeg -i "$input_file" |& rget '^\s*title\s*:\s*(.+)')" || true
  assert test -n "$title" @RET

  artist="$(ffmpeg -i "$input_file" |& rget '^\s*artists?\s*:\s*(.+)')" || true

  local new_filename="${title}"

  if test -n "$artist" ; then
      new_filename+=" by ${artist}"
  fi

  new_filename="$(str-normalize "${new_filename}")" @TRET
  new_filename="$(str2filename "${new_filename}")" @TRET
  # re var-show title artist new_filename
  assert test -n "${new_filename}" @RET

  if test -n "$format" ; then
      new_filename+=".${format}"
  fi

  revaldbg command gmv -v -i "$input_file" "$new_filename"
}
##
