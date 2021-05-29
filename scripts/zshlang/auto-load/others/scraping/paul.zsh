function paulg() {
    getlinks http://www.paulgraham.com/articles.html | rg http://www.paulgraham.com | filter match-url | uniq -u
}
