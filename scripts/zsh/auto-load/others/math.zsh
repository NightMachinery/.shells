calc-raw() {
    python3 -c "from math import *; print($*)"
}
ialias calc='noglob calc-raw'
ialias x='noglob calc-raw'
