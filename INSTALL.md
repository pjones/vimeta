# Installing Vimeta

  * If you already have a Haskell environment installed:

        cabal install vimeta

  * Starting from scratch:

    1. Install the [Haskell Platform](https://www.haskell.org/platform/)

    2. Then use `cabal` to install `vimeta`:

            cabal update
            cabal install vimeta

## Creating the `man` Page

    pandoc -s -t man README.md -o vimeta.1
