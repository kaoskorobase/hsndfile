# HSndfile

HSndfile is a Haskell interface to Eric de Castro Lopo's [libsndfile][1].

# Build requirements

* ghc 6.X.X (tested with ghc-6.8.1)
* c2hs

# Build instructions

Build the library

    $ runhaskell Setup.hs configure
    $ runhaskell Setup.hs build

Build the documentation

    $ runhaskell Setup.hs haddock

# Installation

    # runhaskell Setup.hs install

# Usage

The interface is very similar to [libsndfile's][1] C API, although some
changes were made in order to conform to Haskell naming conventions.

# TODO

* TODO: sf_command interface

[1]: http://www.mega-nerd.com/libsndfile/
