##
function javac-dir {
    local in_dir="${1:-src}" out_dir="${2:-out}"
    assert-args in_dir out_dir

    ##
    # local java_files
    # java_files="$(fd --type=file --glob '*.java')" @TRET

    # reval-ec javac -d "$out_dir" '@'<(ec "$java_files") @RET
    ##
    silent trs-rm "$out_dir" @TRET #: otherwise would not recompile unless =Main.java= changes
    mkdir -p -- "$out_dir" @TRET

    reval-ec javac -d "$out_dir" -sourcepath "$in_dir" "${in_dir}/Main.java" @RET
    ##
}

function java-run {
    local class_name="${1:-Main}" out_dir="${2:-out}"
    assert-args class_name out_dir

    java -classpath "$out_dir" "$class_name"
}
##
function java-project-to-single-file {
    local src_dir="${1:-src}" out="${2:-tmp/Main.java}"
    assert test -e "$src_dir" @RET

    ensure-dir "$out" @TRET

    local content=""
    local pre=""
    local f import_regex='^\s*import\s'
    for f in "$src_dir"/**/*.java(.DN) ; do
        pre+=$'\n\n'
        pre+="$(cat "$f" |
            rg "$import_regex")" || true

        content+=$'\n\n'
        content+="$(cat "$f" |
            rg -v "$import_regex" |
            perl -pe 's/public\s+(class|enum|interface)/$1/g' |
            sd --string-mode 'boolean QUERA_P = false' 'boolean QUERA_P = true' |
            sd 'class Main' 'public class Main')" @TRET
    done

    {
        ec "$pre"
        ec $'\n'"///////////// content:"
        ec "$content"
    } > "$out" @RET
}
##
