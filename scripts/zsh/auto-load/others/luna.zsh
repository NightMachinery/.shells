alias lunar='lo_s=$((60*${1:-36})) lo_noinit=y lo_p=${2:-~/tmp/.luna} loop'
luna() {
    lunar pmset displaysleepnow
}
lunas() {
    lunar redo say "disquiet creatures of avarice have risen yet again ..." 22 
}
alias lq='loop-startover ~/tmp/.luna'
