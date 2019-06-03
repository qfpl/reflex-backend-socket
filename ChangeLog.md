# Revision history for reflex-backend-socket

## 0.2.0.0  -- 2019-06-03

* Decoupled reflex-binary from this library. If you want incremental
  decoding, you can feed the `ByteString`s into reflex-binary
  yourself.

* Simplified, rewrote and documented almost all of the library.

* Use microlens-th to generate lenses for smaller dependency
  footprint.

## 0.1.0.0  -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
