#!/usr/bin/env zsh

function julia-setup-packages {
    local pkgs=(
        IJulia

        Revise
        InteractiveUtils
        InteractiveCodeSearch
        REPL

        BenchmarkTools
        Infiltrator
        FreqTables
        RDatasets
        Lazy
        UUIDs
        Printf
        Distributions

        Flux
        Zygote
        BSON
        StatsBase
        UnicodePlots

        Colors
    )

    julia_add_p='' julia.add "$pkgs[@]"
}

julia-setup-packages
