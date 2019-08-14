warm-zlua() {
    fc -ln 0|grep -o "^cd [~/].*"|sed -e "s|cd ||" -e "s|~|$HOME|" -e 's|\\ | |' -e "s|/$||"|sort|uniq|while read -r d; do test -d "$d" && echo "$d|1|0"; done >> ~/.zlua
}
