cabal build &&
find test -name '*.txt' -exec './dist/build/krunimir/krunimir' '{}' $1 ';'
