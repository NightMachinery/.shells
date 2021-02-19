function fuzzy-choose() {
    : "Usage: arrN <choice> ... | fuzzy-choose query # returns candidates in order"
    fuzzyChoose.js "$*" | jqm '.[] | .[1]'
}
