##
function xkcd() {
    wget `wget -qO- dynamic.xkcd.com/comic/random | sed -n 's/Image URL.*: *\(\(https\?:\/\/\)\?\([\da-z\.-]\+\)\.\([a-z\.]\{2,6\}\)\([\/\w_\.-]*\)*\/\?\)/\1/p'`
}
##
