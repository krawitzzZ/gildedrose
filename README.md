# Haskell port of the Gilded-Rose Kata

This is a Haskell port of the [_Gilded-Rose-Kata_](https://github.com/emilybache/GildedRose-Refactoring-Kata).

## Prerequisite

If you don't have a recent Stack version installed in your system, follow the
[installation instructions](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
for your operating system.

## Building and Running

Run `stack build` initially, then to execute the program run `stack run [days]` where `[days]`
denotes an optional parameter for the number of days to simulate (20 by default).

Output of the initial code is in the `gold-master.txt` file. Output of the refactored and extended code
(with `Conjured` item added) is in the `gold-master-refactored.txt` file.

## Testing

To execute the tests run `stack test`.

Tests are in `test/GildedRoseSpec.hs`. Refer to http://hspec.github.io/ for
more information about writing tests using `Hspec`.
