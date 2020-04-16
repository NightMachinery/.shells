candidate-aliases() {
    fc -l 1 9999999 | awk '{print $2" "$3}' | sort | uniq -c | sort -n
    # fc -l 1 = history (in bash?)
}
