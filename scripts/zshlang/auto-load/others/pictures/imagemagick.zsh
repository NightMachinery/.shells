##
function magick-convert {
  magick convert "$@"
  # @? use =mogrify= instead of =convert= to do it inplace
}
##
