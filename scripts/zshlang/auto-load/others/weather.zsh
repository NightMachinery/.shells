##
# @todo refactor weather-get out of wallpaper-set-darwin
##
function wt1() {
    curl -s 'wttr.in/{'"${1:-Tehran,سبزوار,کیش,Mashhad,نمک‌آبرود,اردبیل}"'}?format="%l:+%C+%c+%t+%h+%w+%m+%M+%p"&m'
}
function weather-short() {
    curl -s 'wttr.in/{'سبزوار'}?format=%l:+%C+%t+%h+%w+%m+%M+%p&m' @RET
    ec
}
