##
function llm-local {
    llm -m starling-7b "$@"
}
##
function datenat-llm {
    ##
    # The results aren't good. It's also slow and somewhat insecure.
    ##
    # llm-local "What date is the second last Thursday 13:45PM? Give a Python script that outputs the answer."$'\n''```python'$'\n'
    ##
    local query="$*"

    local log
    log="$(gmktemp)" @TRET

    {
        local code
        code="$(datenat_llm.py "${query}" 2>$log)" || {
            cat "$log" >&2
            return 1
        }

        ecgray "${code}"

        python -c "${code}"
    } always {
        silent trs-rm "${log}"
    }
    ##
}
##
