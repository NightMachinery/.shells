colorfb-r() {
    local hue="$(random-color -f hex)"
    # hue=random
    reval chalk -t "{$(random-color -l dark --hue "$hue").bgHex('$(random-color -f hex -l light --hue "$hue")') $*}"
}
cc88 () {
    reval chalk -t "{hex('$(random-color -f hex -l dark)').bgHex('$(random-color-f -f hex -l light)') $*}"
}
random-color-test() {
    node -e "var randomColor = require('randomcolor'); // import the script
var color = randomColor({count: 1}); // a hex code for an attractive color
console.log(JSON.stringify(color))"|jq -re '.[]'
}
