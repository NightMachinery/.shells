function h_dhint() {
    pandoc -f markdown <(curl  --silent --fail --location "https://github.com/rstacruz/cheatsheets/raw/master/$1.md")
}
function dhint() {
    # devhints.io

    # md.py did not render some stuff , e.g. vim, well
    # md.py =(curlm "https://github.com/rstacruz/cheatsheets/raw/master/$1.md") | bt

    # mdw but we couldn't memoi that
    memoi_expire=0 eval-memoi h_dhint "$1" | skipin w3m -T text/html
}
##
function months-greg {
    ec "1.  January - 31 days
2.  February - 28 days in a common year and 29 days in leap years
3.  March - 31 days
4.  April - 30 days
5.  May - 31 days
6.  June - 30 days
7.  July - 31 days
8.  August - 31 days
9.  September - 30 days
10. October - 31 days
11. November - 30 days
12. December - 31 days"
}

function months-jalali {
    wh monthj2en | cat
}

function months {
    months-greg
    months-jalali
}
##
ch-mpv() icat $nightNotes/cheatsheets/mpv/mpbindings.png
##
