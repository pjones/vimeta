# Vimeta -- A video metadata batch editor

Vimeta is a tool to fetch video metadata from the interwebs and update
video files.  For movies, metadata is pulled from [TheMovieDB][] while
[TheTVDB][] is used for TV episodes.

[AtomicParsley][] is currently used to update the metadata in a file.

This tool isn't very user friendly.  There are no current plans to
change this but patches are welcome.

# Installation

Dependencies:

  * The [Haskell Platform][].

  * [AtomicParsley] installed and in your `PATH`.

  * Haskell [library][hsm] for TheMovieDB.

  * Haskell [library][hst] for TheTVDB.

Installation:

    cabal configure && cabal build && cabal install

# Usage

## Get API Keys

Before you can use `vimeta` you need to create accounts and request
API keys from [TheMovieDB] and [TheTVDB].

  * Place your [TheMovieDB] API key in: `~/.tmdbkey`

  * Place your [TheTVDB] API key in: `~/.tvdbkey`

## Movies

  1. Fetch the movie ID from [TheMovieDB][]:

        tmdb search "back to the future"

  2. Update the video metadata:

        vimeta movie -i 105 file.m4v

## TV Episodes

Assuming that you have two files for season 3 episodes 4 and 5:

  1. Fetch the series ID from [TheTVDB][]:

        tvdb search "how I met your mother"

  2. Update the metadata in the files:

        vimeta tv -i 75760 -s 3 -e 4 episode4.m4v episode5.m4v

[TheMovieDB]: http://www.themoviedb.org/
[TheTVDB]: http://thetvdb.com/
[AtomicParsley]: http://atomicparsley.sourceforge.net/
[Haskell Platform]: http://www.haskell.org/platform/
[hsm]: https://github.com/pjones/themoviedb
[hst]: https://github.com/pjones/thetvdb
