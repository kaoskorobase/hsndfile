# hsndfile

**hsndfile** is a Haskell interface to Eric de Castro Lopo's [libsndfile][].

# Build requirements

* ghc 6.X.X, tested with
	* ghc-6.8.1
	* ghc-6.10.1
* c2hs

# Installation instructions (simple)

The easiest way to install **hsndfile** is by using [cabal-install][]:

		$ cabal-install hsndfile

Yes, that's all! If you want to play with the code, more detailed instructions
for getting the code and building **hsndfile** are provided in the next
section.

# Installation instructions (detailed)

## Getting the code

* download the [latest release from hackage][hackage]
* get a copy of the darcs repository
		
		$ darcs get http://code.haskell.org/hsndfile/

## Build instructions

Build the library

    $ runhaskell Setup.hs configure
    $ runhaskell Setup.hs build

Build the documentation

    $ runhaskell Setup.hs haddock

## Installation

    # runhaskell Setup.hs install

# Usage

The interface is very similar to [libsndfile's][libsndfile] C API, although some
changes were made in order to conform to Haskell naming conventions. Have a
look at the [API documentation][], most of which is copied from the original C
header file.

# TODO

* TODO: sf_command interface

[libsndfile]: http://www.mega-nerd.com/libsndfile/
[hackage]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/hsndfile
[cabal-install]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/cabal-install
[API documentation]: http://hackage.haskell.org/packages/archive/hsndfile/0.3.3/doc/html/Sound-File-Sndfile.html
