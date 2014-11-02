# libarchive-conduit
`libarchive-conduit` reads archives with `libarchive`. All of the many
formats understood by `libarchive` are supported. Resource use is
managed in Haskell with the `conduit` library. The interface is very
simple; archives are read from disk and their contents are presende as
a stream of pairs `(FilePath, ByteString)` of the path to each file
and its contents, respectively.

## Limitations

The only major limitation the author is aware of is on file
size. Strict `ByteString`s are used to retrieve the contents of
individual files within an archive, so no individual file may be
larger than RAM. Inspecting the archive contents lazily is problematic
because `libarchive` only allows streaming archives in order. If lazy
`ByteString`s were used, the contents of the archive might be
evaluated out of order. There is no limitation on the size of the
archive, though.

There are also several minor limitations:

* Only the file names and contents are extracted from the archive,
  although `libarchive` will provide all the file status information
  (modification time, permissions, etc.).
* Writing archives is not implemented, although `libarchive` supports this.
* Extracting files to disk is not implemented, although `libarchive`
  supports this.
* Archives must be stored in a file, although `libarchive` also
  supports reading files from memory.

There is no reason, in principle, that these limitations cannot be
addressed. The author simply hasn't implemented these features yet
because he doesn't need them.
