function dblp-relevant-2025 {
    #: @alt [[NIGHTDIR:python/dblp/dblp_relevance_2025.py]]
    ##
    local url="${1}"
    assert-args url @RET
    shift

    local titles
    titles="$( dblp-titles "${url}" |
        duplicates_clean_case_sensitive=n duplicates-clean)" @RET

    local color_p=n
    {
        ec "$titles" |
            rgbase 'interp|explain|salien|attribution|understand|concept|visualiz'

        ec-sep-h 2>&1

        ec "$titles" |
            rgbase 'relevan|ground|\bwhy\b|mechanis|circuit|probe|probing|atten'

        ec-sep-h 2>&1

        ec "$titles" |
            rgbase 'robust|adversarial'

        ec-sep-h 2>&1

        ec "$titles" |
            rgbase 'vision transformer|reason|agent|llm|language model'
    } |
        duplicates_clean_exceptions=("$(ec-sep-h 2>&1 | tail -n2)" '-----------') duplicates_clean_case_sensitive=n duplicates-clean
}
