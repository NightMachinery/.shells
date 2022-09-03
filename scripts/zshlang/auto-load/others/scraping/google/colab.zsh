function colab-from-github {
    local inargs
    inargs="$(in-or-args "$@")" @RET

    #: "https://github.com/fastai/course-nlp/blob/master/2b-odds-and-ends.ipynb" ->
    #: "https://colab.research.google.com/github/fastai/course-nlp/blob/master/2b-odds-and-ends.ipynb"

    ec "$inargs" |
        perl -pe "s|^https://github.com/|https://colab.research.google.com/github/|g" |
        prefixer --skip-empty |
        cat-copy-if-tty
}
