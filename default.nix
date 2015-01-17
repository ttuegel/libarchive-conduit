{ mkDerivation, archive, base, bytestring, conduit, resourcet
, stdenv, transformers
}:
mkDerivation {
  pname = "libarchive-conduit";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [ base bytestring conduit resourcet transformers ];
  extraLibraries = [ archive ];
  description = "Read many archive formats with libarchive and conduit";
  license = stdenv.lib.licenses.bsd3;
}
