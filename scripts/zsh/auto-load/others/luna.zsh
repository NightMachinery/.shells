luna() {
    lo_s=$((60*${1:-30})) lo_noinit=y lo_p=${2:-~/tmp/luna} loop pmset displaysleepnow
}
alias lq='loop-startover ~/tmp/luna'
