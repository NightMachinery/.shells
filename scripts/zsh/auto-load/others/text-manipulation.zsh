### Module Text Manipulation
### This module specializes in functions that do not touch the disk.
###
dedent() {
    sd --flags m '^\s*' ''
}
trim() {
	gsed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//'
}
