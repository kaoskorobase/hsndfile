### [v0.7](https://github.com/kaoskorobase/hsndfile/tree/v0.7.1)

* Read and write signed instead of unsigned integer samples and rename `sf_readf_wordXX` and `sf_writef_wordXX` to `sf_readf_intXX` and `sf_writef_intXX`, respectively
* Add new supported header and sample formats

### v0.6

* Don't export `Control.Exception.catch` and `Prelude` from `Sound.File.Sndfile`

### v0.5

* Remove lazy read functions `hGetContentChunks` and `readFileChunks` from library interface. Those functions were implemented incorrectly.

### v0.4

* Simplified Buffer API: A single type class, **Buffer**, is provided for **ForeignPtr** based I/O. Instances are provided in separate packages, e.g. [hsndfile-vector](http://hackage.haskell.org/package/hsndfile-vector).

### v0.3.2

* **hsndfile** has been adapted to compile with GHC 6.10. The only
  visible change is in exception handling:
    * `Sound.File.Sndfile.Exception.Exception` is now an instance of
      `Control.Exception.Exception`
    * The new generalized functions from `Control.Exception.Exception` are
      used for throwing and handling exceptions

### v0.2.0

* Fix exception throwing Exceptions detected in library code are
  now actually raised. Exception has been factored into
  Sound.File.Sndfile.Exception and constructors were added according to the
  public libsndfile error codes.
* Fix reading/writing of frames hGetFrames and hPutFrames were
  using the sample-based library functions. These have been factored into
  Sound.File.Sndfile.Buffer and the correct functions are being used for the
  frame-based I/O functions.
