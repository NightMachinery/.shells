autoload -U colors && colors

## Functions
colorfg() { ! isI || printf "\x1b[38;2;${1:-0};${2:-0};${3:-0}m" }
colorbg() { ! isI || printf "\x1b[48;2;${1:-0};${2:-0};${3:-0}m" }
colorb() {
    co_f=colorbg color "$@"
}
color() {
    local in
    local noreset="$coNr"
    local nonewline="$coN"
    comment "Note that we need to first get stdin and then print the color, otherwise we'll print the color before anything has been outputted, resulting in race conditions."
    [[ "$1" =~ '^\d+$' ]] &&
        {
            in="$(in-or-args "${@[4,-1]}")"
            "${co_f:-colorfg}" "$@"
        } || {
            in="$(in-or-args "${@[2,-1]}")"
            isI && printf %s "$fg[$1]"
        }
    print -nr -- "$in"
    test -n "$noreset" || resetcolor
    test -n "$nonewline" || echo
}
resetcolor() {
    comment This var is seemingly coming from a plugin or sth
    ! isI || printf %s "$reset_color"
}
helloworld() {
    colorbg 0 0 255;colorfg 0 255; ec HELLO "$(colorfg 255 100)"BRAVE"$(colorfg 0 255)" $(colorbg 100 0 255)NEW$(colorbg 0 0 255) WORLD\!;resetcolor
}
printcolors() {
    printf "\x1b[${bg};2;${red};${green};${blue}m\n"
    helloworld
    comment awk 'BEGIN{
    s="/\\/\\/\\/\\/\\"; s=s s s s s s s s;
    for (colnum = 0; colnum<77; colnum++) {
        r = 255-(colnum*255/76);
        g = (colnum*510/76);
        b = (colnum*255/76);
        if (g>255) g = 510-g;
        printf "\033[48;2;%d;%d;%dm", r,g,b;
        printf "\033[38;2;%d;%d;%dm", 255-r,255-g,255-b;
        printf "%s\033[0m", substr(s,colnum+1,1);
    }
    printf "\n";
}'
    ec 'https://github.com/johan/zsh/blob/master/Functions/Misc/colors
# Text color codes:
  30 black                  40 bg-black
  31 red                    41 bg-red
  32 green                  42 bg-green
  33 yellow                 43 bg-yellow
  34 blue                   44 bg-blue
  35 magenta                45 bg-magenta
  36 cyan                   46 bg-cyan
  37 white                  47 bg-white
# 38 iso-8316-6           # 48 bg-iso-8316-6
  39 default                49 bg-default'
}
random-color() {
    randomColor.js "$@" |jq -re '.'
    # --seed "$(head -c 100 /dev/random)" 
}
random-color-arr() {
	#shuf -i 0-255 -n 3
	# subshell doesn't change OUR seed. #ec $(($RANDOM % 256)) $(($RANDOM % 256)) $(($RANDOM % 256))
	[[ "$(randomColor.js -f rgbArray "$@")" =~ '\[(\d+),(\d+),(\d+)\]' ]] && ec "$match[@]"
}
ecrainbow-n() {
	local hue="$(random-color -f hex)"
	print -nr -- "$(colorfg $(random-color-arr -l dark --hue "$hue"))$(colorbg $(random-color-arr -l light --hue "$hue"))""$@"
}
ecrainbow() { ecrainbow-n "$@" ; echo }
ecalt1() { print -nr -- "$(colorfg 0 255 100)$(colorbg 255 255 255)${*:-EMPTY_HERE} " }
ecalt2() { print -nr -- "$(colorfg 255 255 255)$(colorbg 0 255 100)${*:-EMPTY_HERE} " }
ecalternate() {
	(($#)) || { resetcolor ; echo ; return 0 }
	ecalt1 "$1"
	shift 1
	(($#)) || { resetcolor ; echo ; return 0 }
	ecalt2 "$1"
	shift 1
	$0 "$@"	
}
