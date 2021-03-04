function natgeo-wallpaper-get() {
    withchrome full-html2 https://www.nationalgeographic.com/photography/photo-of-the-day/ |  pup 'source' | urls-extract | command rg '1900' # | inargsf icat
}
