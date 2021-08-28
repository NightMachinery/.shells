#!/usr/bin/env runhaskell
-- delink.hs
import Text.Pandoc.JSON -- needs `cabal v2-update && cabal v2-install --lib pandoc-types`

main = toJSONFilter delink

delink :: Inline -> [Inline]
delink (Link _ txt _) = txt
delink x              = [x]
