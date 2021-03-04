function wt1() {
    curl -s 'wttr.in/{'"${1:-Tehran,سبزوار,کیش,Mashhad,نمک‌آبرود,اردبیل}"'}?format="%l:+%C+%c+%t+%h+%w+%m+%M+%p"&m'
}
