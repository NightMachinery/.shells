#!/usr/bin/env zsh

local perl_installables=('App::ansifold' 'Text::ASCIITable')

local i
for i in $perl_installables[@] ; do
    cpanm "$i"
done
