typeset -g pandoc_md_default='markdown+tex_math_single_backslash+lists_without_preceding_blankline'
##
function latex2md-pandoc {
    @opts from 'latex+latex_macros' to markdown @ pandoc-convert "$@"
}

function latex2plain-pandoc {
    @opts from 'latex+latex_macros' to plain @ pandoc-convert "$@"
}
##
function epub2org() {
    @opts from epub to org trim_extra n @ pandoc-convert "$@"
}

function epub2md {
    @opts from epub to markdown @ pandoc-convert "$@"
}

function epub2plain() {
    @opts from epub to plain @ pandoc-convert "$@"
}

function docx2org() {
    @opts from docx to org @ pandoc-convert "$@"
}

function md2org {
    @opts from "${pandoc_md_default}" to org @ pandoc-convert "$@"
}

function mediawiki2md {
    @opts from mediawiki to markdown @ pandoc-convert "$@"
}

function mediawiki2org {
    @opts from mediawiki to org @ pandoc-convert "$@"
}

function md2plain() {
    @opts from "${pandoc_md_default}" to plain @ pandoc-convert "$@"
}

function org-heading-to-list-item {
    in-or-args "$@" |
        org-header-rm-shared-level |
        perl -CS -lpe 'BEGIN { use utf8; use open qw/:std :utf8/; } ; s/^(\*+)(\s)/"  " x (length($1) - 1) . "-" . $2/e' |
        cat-copy-if-tty
}

function org2md {
    in-or-args "$@" |
    perl -lpe 's/^(#\+[a-zA-Z]+_example)\s+.*/$1/gi' | #: @upstreamBug pandoc doesn't convert example blocks correctly
        org2md-raw |
        perl -lpe 's/^(```)\s+example$/$1/g' |
        perl -lpe 's/\\([][@])/$1/g' |
        perl -lpe 's/\[{1,2}jalali:([^][]+)\]{1,2}/$1/g' |
        perl -lpe "s/\\\\('|\")/\$1/g" |
        cat-copy-if-tty
}

function org2md-raw {
    @opts from org to "${pandoc_md_default}" @ pandoc-convert "$@"
}

function org2json() {
    @opts from org to json @ pandoc-convert "$@"
}

function json2org() {
    @opts from json to org @ pandoc-convert "$@"
}

function org2plain() {
    @opts from org to plain @ pandoc-convert "$@"
}

function org2html {
    @opts from org to html @ pandoc-convert "$@"
}

function pbcopy-org2html {
    org2html "$@" |
        pbcopy-html
}

function md2html {
    # md2org | org2html
    @opts from "${pandoc_md_default}" to html @ pandoc-convert "$@"
}

function pbcopy-md2html {
    md2html "$@" |
        pbcopy-html
}

function org2rtf() {
    @opts from org to rtf @ pandoc-convert "$@"
}

function md2html-fancy {
    #: @todo Write zsh function `md2html-fancy` which converts markdown into beautiful html. You can use third-party tools such as pandoc.
    ##
    @NA
}


function html2rtf-textutil() {
    # @darwinOnly?
    textutil -stdin -stdout -format html -convert rtf
}

function rtf2txt-1 {
    unrtf "$@" | html2text
}
##
function html2org {
    local input="${1}" o_format="${html2org_f:-org}"
    if test -z "$input" ; then
        input="$(gmktemp)"

        if isInTty ; then
            pbpaste-html > "$input" @TRET
        else
            local input_text
            input_text="$(cat)" @TRET
            if isBrish && test -z "$input_text" ; then
                pbpaste-html > "$input" @TRET
            else
                ec "$input_text" > "$input" @TRET
            fi
        fi
    fi

    @opts from html to "$o_format" @ pandoc-convert "$input" "${@[2,-1]}"
}

function html2plain {
    @opts f plain @ html2org "$@"
}

function html2plain-std {
    # somewhat faster than html2plain, but less flexible
    pandoc --wrap=none --from html --to plain - -o -
}

function html2md-v1 {
    @opts f "${pandoc_md_default}" @ html2org "$@"
}
# @opts-setprefix html2md html2org

function html2md-v2 {
    html2org | org2md
    #: This strips a lot of useless metadata compared to =html2md=.
}

function html2md-strict {
    @opts f markdown_strict @ html2org "$@"
}
aliasfn html2md html2md-strict

function html2md-djot {
    #: djot still outputs superfluous stuff
    ##
    @opts f djot @ html2org "$@"
}

function html2md-gfm {
    #: gfm outputs even more superfluous stuff
    ##
    @opts f gfm @ html2org "$@"
}

function html2md-commonmark {
    #: commonmark outputs even more superfluous stuff
    ##
    @opts f commonmark @ html2org "$@"
}

function html2md-commonmark-x {
    #: commonmark_x still outputs superfluous stuff
    ##
    @opts f commonmark_x @ html2org "$@"
}

function html2text {
    # @alt/worse html2text (python)
    ##
    html2org =(cat)
}
##
function pandoc-convert {
    ensure-array pandoc_opts
    local input="${1}" output="${2:--}" from="${pandoc_convert_from}" to="${pandoc_convert_to}" trim_extra="${pandoc_convert_trim_extra:-y}" opts=("$pandoc_opts[@]") delink="${pandoc_delink}" postprocess_p="${pandoc_convert_postprocess_p:-y}"
    assert-args from to @RET
    if bool $delink ; then
        ##
        # opts+=(--filter 'delink.hs') # must be on PATH
        ##
        # opts+=(--filter 'pandoc_delink.py') # must be on PATH
        ##
        opts+=(--filter 'panflute_delink.py') # must be on PATH
    fi

    if test -z "$input" ; then
        input="$(gmktemp)" @TRET

        if isInTty ; then
            assert pbpaste > "$input" @RET
        else
            assert cat > "$input" @RET
        fi
    fi
    tmp_o="$(gmktemp)" @TRET


    if [[ "$to" =~ '^(?:gfm|commonmark.*|markdown.*|djot)$' ]] ; then
        #: [[id:1ffc4d15-5baf-44bd-9cd4-98d50b3270b4][Add option to disable indented code blocks? · Issue #2120 · jgm/pandoc]]
        opts+=(--lua-filter="${NIGHTDIR}/python/pandoc_filters/md_code_blocks.lua")
        # var-show opts
    fi

    pandoc --wrap=none "$opts[@]" --from "$from" --to "$to" "$input" -o "-" | {
        if bool ${postprocess_p} ; then
            if [[ "$to" == org ]] ; then
                pandoc-normalize-whitespace | {
                    if bool $trim_extra ; then
                        pandoc-org-trim-extra | org-trim-forced-newlines
                    else
                        cat
                    fi
                } |
                    org-remove-inline-images |
                    perl -CS -lpe 's/^(\s*\d+\.\s+)\[@\d+\]\s/$1/g' |
                    perl -CS -lpe 's{\[cite/t:([^][]+)\]}{$1}g' |
                    perl -CS -lpe 's{^(\s*#\+(?:begin|end)_)example\b}{lc($1) . "src"}ieg' | #: =lc= converts to lower-case
                #: =example= blocks are produced from markdown source blocks with no language identifier. These blocks will not be syntax-highlighted when exported to HTML, so I am converting them to source blocks instead.
                    perl -CS -lpe 's{^(\s*#\+begin_src\b.*)$}{$1 :eval never}ig' |
                #: We can add a negative lookbehind to avoid adding =:eval never= if it's already added, but I don't think such a check is needed.
                    perl -CS -lpe '/^\s*#\+begin_src\b/i && s/:style.*?(?:$|\s(*pla::))//ig'
                #: Removes `:style ...` till the next attribute, hence `\s(*pla::)`. (Note the non-greedy matcher.)

            elif [[ "$to" =~ '^markdown' ]] && bool $trim_extra ; then
                perl -0777 -pe 's/(?<=\W)\{(?:\.|\#)[^{}]+\}(?=\W)//g' |
                    perl -0777 -pe 's/\{ref\}(?:=)//g'
                # trying to remove header_attributes, fenced_code_attributes, inline_code_attributes, etc
                # @alt https://stackoverflow.com/questions/42070656/pandoc-html-to-markdown-remove-all-attributes
                # @alt i always converted from markdown, but never to. maybe the same pattern: target+ext1-ext2. if not, maybe select another output format like markdown_strict.
                # @example Is `item_tfms`{.verbatim} applied separately on each epoch?
            elif [[ "$to" == plain ]] ; then
                perl -CS -lpe 's{\x{2013}}{--}g'
                #: -> perl -CS -lpe 's{–}{--}g'
                #: pandoc somehow changes '--' even when in example blocks
            else
                cat
            fi
        else
            cat
        fi
    } > "$tmp_o" @TRET # noflycheck

    if [[ "$output" != '-' ]] ; then
        assert command mv "$tmp_o" "$output" @RET
    else
        if isIReally && isOutTty ; then # =@opts p [ org2 html2 md2 ] @ fn-isTop=
            cat "$tmp_o" | pbcopy @TRET
        fi
        assert cat "$tmp_o" @RET
        assert silent trs-rm "$tmp_o" @RET
    fi
}
##
function pandoc-org-trim-extra {
    local pandoc_org_extra_regex
    pandoc_org_extra_regex='(?:\n|\A)\h*(?:(?::PROPERTIES:(?:.|\n)*?:END:)|(?:<<.*?>>))\h*|\^\{\/\/\}'

    perl -0777 -pe "s/${pandoc_org_extra_regex}//g"

    # [[nightNotes:private/playgrounds/pandoc.zsh::perl -0777 -pe][perl]]
}

function org-trim-forced-newlines {
    # @duplicateCode/1102efd8dd0367b8dc12d7e3678dca59
    sd '\\\\' ''
}


function pandoc-normalize-whitespace() {
    # @duplicateCode/ed0e38095407ff82d0f12a431c3c10a2
    gsed 's/ / /g'
    # pandoc uses some bad whitespace (U+00A0)(#\NO-BREAK_SPACE) that cannot be read by org-mode
    #
    # https://superuser.com/questions/517847/use-sed-to-replace-nbsp-160-hex-00a0-octal-240-non-breaking-space
}

## Deprecated as html2org does this already:
# function pop-html2org() {
#     reval-copy pandoc --from html --to org =(pbpaste) -o -
# }
# aliasfn p-h2o pop-html2org
##
function doc2org {
    : "pandoc only supports DOCX"

    local inputs=($@)

    reval-ec antiword -f "$inputs[@]"
    #        -f     Output in formatted text form. That means that  bold  text  is printed  like  *bold*,  italics  like /italics/ and underlined text as _underlined_.
}
##
