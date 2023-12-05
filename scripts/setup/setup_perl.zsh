#!/usr/bin/env zsh

local perl_installables=(
    'App::ansifold' #: @keywords perl cpanm install ansifold
    'Text::ASCIITable'
    'Text::Unidecode'
    'Lingua::Translit'
    'Data::Printer'
)

local i
for i in $perl_installables[@] ; do
    cpanm "$i"
done

ansifold-path-fix
