function bing-wallpaper-get() {
    local url
    url="$(full-html2 https://www.bing.com/ | pup 'link[as="image"] attr{href}')" || return $?
    ec "https://www.bing.com/$url"
}
