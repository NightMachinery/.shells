#!/usr/bin/env zsh

pushf ~/bin
{
    sbcl_batteriful.lisp
    ##
    hello_cl_c.lisp
    ## migrated to Go/image executors:
    # tag-filter-date.lisp
    # html_links_textualize.lisp
    ##
} always { popf }
