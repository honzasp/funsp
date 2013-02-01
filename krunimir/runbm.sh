cabal build &&
find benchmark -name '*.txt' -exec './dist/build/krunimir/krunimir' '{}' $1 +RTS $RTS ';'

