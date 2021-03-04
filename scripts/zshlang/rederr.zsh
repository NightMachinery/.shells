## colorize std_err
zmodload zsh/terminfo zsh/system
autoload colors is-at-least
if [[ "${terminfo[colors]}" -ge 8 ]] { colors }
color_err () {
   ## sysread & syswrite are part of zsh/system
    while sysread std_err_color
    do
      syswrite -o 2 "${fg_bold[red]}${std_err_color}${terminfo[sgr0]}"
    done
}
## i'm not sure exactly how far back it's safe to go with this
## 4.3.4 works; 4.2.1 hangs.
is-at-least 4.3.4 && exec 2> >( color_err )
