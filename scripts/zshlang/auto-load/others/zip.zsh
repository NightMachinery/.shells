##
function zir {
    local dest="${$(removeTrailingSlashes "$1"):r}$2".zip
    \rm "$dest" &> /dev/null
    zip -r "$dest" "$1"
}

function prefix-shared-dir-get {
  local input
  input="$(in-or-args "$@")" @RET
  dact var-show input

  local res
  res="$(ec "${input}" |
    prefixer --skip-empty |
    gsort -u)" @TRET

  ec "$res" |
        perl -e '
        use File::Basename;

        # my @paths = <STDIN>;
        my @paths = grep { $_ ne "" } <STDIN>;
        chomp @paths;
        if (scalar @paths == 1) {
            my $dirname = $paths[0];
            # $dirname =~ s{/[^/]*$}{};
            # $dirname = dirname($path);
            if ($dirname =~ m{^(.*)/[^/]*$}) {
                $dirname = $1;
                print $dirname;
            }

            exit;
        }

        my $prefix = $paths[0];
        if (not ($prefix =~ m|^/|)) {
            $prefix = "";
        }

        for my $path (@paths[1..$#paths]) {
          while (not $path =~ /^\Q$prefix\E/) {
            $prefix =~ s{/[^/]*$}{};

            last if $prefix eq ""; # Break the loop if prefix becomes empty
          }
          last if $prefix eq "";
        }
        print $prefix;
  ' |
    # cat
    cat-copy-if-tty
}

function prefix-shared-dir-strip {
    local paths
    paths="$(in-or-args "$@")" @RET

    local prefix
    prefix="$(ec "$paths" | prefix-shared-dir-get)" @TRET

    ec "$paths" | perl -e '
        my $prefix = shift @ARGV;
        my @paths = <STDIN>;
        chomp @paths;
        for my $path (@paths) {
            $path =~ s{^\Q$prefix\E/*}{};
            print "$path\n";
        }
    ' "$prefix"
}

function zip-create-v1 {
  #: @deprecated
  #: @seeAlso [agfi:zip-flat]
  ##
  local opts=()

  local dest="$1" ; shift
  assert-args dest @RET

  dest="$(grealpath "$dest")" @TRET

  local -a files=("$@")

  local prefix
  prefix="$(re realpath "${files[@]}" | prefix-shared-dir-get)" @TRET
  dact var-show prefix

  local -a pruned_files
  pruned_files=("${(@f)$(prefix-shared-dir-strip "${files[@]}")}") @TRET
  dact var-show pruned_files

  if test -n "$prefix" ; then
      reval-ecgray pushf "$prefix"
  fi
  {
      reval-ecgray zip "${opts[@]}" -r "$dest" ${pruned_files[@]}
  } always {
        if test -n "$prefix" ; then
            popf
        fi
  }
}
##
function zip-flat {
  #: This function puts all of its arguments into the root of the archive.
  #: This is unlike `zip -r`, which will preserve the directory structure of its arguments. I.e., `zip -r dest.zip a/b.txt` will create a file `a/b.txt` in the archive, while `zip-flat dest.zip a/b.txt` will create a file `b.txt` in the archive.
  ##
  #: Check if at least two arguments are provided (destination and one file)
  if (( $# < 2 )); then
    echo "Usage: zip-flat <dest> <file> ..."
    return 1
  fi

  local dest="$1"
  shift 1
  assert-args dest @RET

  if test -e "$dest" ; then
    assert trs "$dest" @RET
  fi

  #: Create a temporary directory to hold the flattened files
  local tmpdir
  tmpdir="$(mktemp -d)" @TRET

  #: Loop over each file or directory to add them to the zip file
  local item
  for item in "$@"; do
    if [[ -d $item ]]; then
      assert command cp -r "$item" "$tmpdir/" @RET
    else
      assert command cp "$item" "$tmpdir/" @RET
    fi
  done

  #: Change to the temporary directory and zip all files from there
  assert pushf "$tmpdir" @RET
  {
    zip -r "$OLDPWD/$dest" ./*
  } always {
    popf

    trs "$tmpdir"
  }
}
##
function unzip2dir() {
    local file="$1" y="${unzip2dir_y:-y}"
    file="$(grealpath -e -- "$file")" @TRET

    local head
    local opts=()
    if [[ "$file" == *.rar ]] && test -n "${commands[unrar]}" ; then
        head='unrar'
        opts+=(-ad)
        # -ad            Append archive name to destination path
    else
        head='7z'
        opts+=(-o"${file:r}/")
    fi

    if bool $y ; then
        opts+='-y'
        # -y     Assume Yes on all queries (works with both 7z and unrar)
    fi

    assert reval-ec "$head" x "$opts[@]" "$file" @RET
    ##
    # unzip "$file" -d "${file:r}/"
    ##
}
reify unzip2dir
aliasfn uzd unzip2dir
##
function archive-list {
    local input="$1"

    ##
    7z l "$input"
    ##
    # unzip -l "$input"
    ##
    # unrar l "$input"
    ##
}
##
