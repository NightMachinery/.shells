##
function zir {
    local dest="${$(removeTrailingSlashes "$1"):r}$2".zip
    \rm "$dest" &> /dev/null
    zip -r "$dest" "$1"
}

function prefix-shared-dir-get {
    in-or-args "$@" |
        prefixer --skip-empty |
        gsort -u |
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
        for my $path (@paths[1..$#paths]) {
          while (not $path =~ /^\Q$prefix\E/) {
            $prefix =~ s{/[^/]*$}{};
          }
        }
        print $prefix;
  ' |
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

function zip-create {
  local opts=()

  local dest="$1" ; shift
  assert-args dest @RET

  dest="$(grealpath "$dest")" @TRET

  local -a files=("$@")

  local prefix
  prefix="$(arrn "${files[@]}" | prefix-shared-dir-get)" @TRET

  local -a pruned_files=("${(@f)$(prefix-shared-dir-strip "${files[@]}")}")

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
