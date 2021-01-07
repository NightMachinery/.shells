function cov() { # covid
    gurl https://coronavirus-19-api.herokuapp.com/countries/"${1:-iran}" | jq .
}
