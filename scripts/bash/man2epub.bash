#!/usr/bin/env bash
# From https://github.com/iustin/man2ebook/blob/master/generate.sh
# Script to generate an epub file containing a list of man pages.
#
# Copyright (C) 2019 Iustin Pop

# rm -rf ./*.html man?/*.html index.*

set -eu

cat > index.html <<EOF
<html>
   <body>
     <h1>Table of Contents</h1>
     <p style="text-indent:0pt">
EOF

for i; do
  mp="$(man -w "$i")"
  bare_i="${i%%.*}"  # without section suffix, since whatis doesn't support it
  outfile="$i.html"
  section="$(basename "$(dirname "$mp")")"
  numsection="${section##man}"
  whatis="$(whatis -s "$numsection" -l "$bare_i")"
  outfile_full="$section/$outfile"
  echo "$i: $mp -> $section/$outfile"
  mkdir -p "$section"
  man2html -r "$mp" > "$section/$outfile"
  cat >> index.html <<EOF
<a href="$outfile_full">$whatis</a><br/>
EOF
done

cat >> index.html <<EOF
     </p>
   </body>
</html>
EOF

echo "Generating epub…"

ebook-convert index.html index.epub \
             --title="Selected man pages" \
             --authors="Man page authors" \
             --breadth-first \
             --page-breaks-before="/" \
             --pubdate="$(date -R)" \
             --max-levels=1 \
             --epub-inline-toc \
             --toc-filter="(Return to Main Contents|Index|NAME|DESCRIPTION|EXAMPLE|SEE ALSO|COLOPHON)" \
             --book-producer="$USER" \
             --comments="Generated from the following list of man pages: $*." \
             --language=en
