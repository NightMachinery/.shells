##
function img-info-magick {
  reval-ec magick identify -verbose "$@"
}

function img-info-exif {
  exiftool "$@"
}

function img-exif-rm {
  : "Remove all EXIF metadata from the given files"

  exiftool -All= "$@"
}
##
function magick-convert {
  magick convert "$@"
  # @? use =mogrify= instead of =convert= to do it inplace
}
##
