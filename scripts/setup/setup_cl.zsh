#!/usr/bin/env zsh

pushf ~/bin
{
    sbcl_batteriful.lisp
    ##
    hello_cl_c.lisp

    tag-filter-date.lisp
    ## migrated to image dump executors:
    # html_links_textualize.lisp
    ##
} always { popf }
