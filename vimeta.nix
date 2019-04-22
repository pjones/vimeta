{ mkDerivation, aeson, base, byline, bytestring, containers
, directory, either, filepath, http-client, http-client-tls
, http-types, mtl, old-locale, optparse-applicative, parsec
, process, stdenv, temporary, text, themoviedb, time
, time-locale-compat, transformers, xdg-basedir, yaml
}:
mkDerivation {
  pname = "vimeta";
  version = "0.3.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base byline bytestring containers directory either filepath
    http-client http-client-tls http-types mtl old-locale
    optparse-applicative parsec process temporary text themoviedb time
    time-locale-compat transformers xdg-basedir yaml
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://code.devalot.com/open/vimeta";
  description = "Frontend for video metadata tagging tools";
  license = stdenv.lib.licenses.bsd2;
}
