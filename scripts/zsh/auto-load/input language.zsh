##
# edit this in vim or sth; emacs's RTL support makes it harder.
export persian_chars="ضصثقفغعهخحجچشسیبلاتنمکگظطزرذدپو.؛؟٬٫﷼٪×،ـ"
export en_chars="qwertyuiop[]asdfghjkl;'zxcvbnm,.\"?@#\$%^&_"
##
function per2en() {
    gsed "y/$persian_chars/$en_chars/"
}
aliasfn p2e per2en
function ppe() { pbpaste | reval-copy per2en }

function en2per() {
    gsed "y/$en_chars/$persian_chars/"
}
aliasfn e2p en2per
function pep() { pbpaste | reval-copy en2per }
