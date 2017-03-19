% VIMETA(1) Vimeta User Manual
% Peter J. Jones
% May 19, 2015

# NAME

vimeta - frontend for video metadata tagging tools

[![Build Status](https://travis-ci.org/pjones/vimeta.svg?branch=master)](https://travis-ci.org/pjones/vimeta)

# SYNOPSIS

vimeta --version

vimeta config [*options*]

vimeta movie [*options*] *FILE*

vimeta tv [*options*] [*FILE*]...

# DESCRIPTION

Vimeta is a tool to fetch video metadata from the interwebs and update
video files using a tagging tool.

[TheMovieDB][] is used as the source for movie and TV series metadata.
This means that an API key for TheMovieDB is required in order to use
Vimeta.  Sign up for an account at TheMovieDB.com and create an API
key.  See the *config* command for details on how to create a
configuration file that includes your API key.

[AtomicParsley][] is the default tagging tool used to update the
metadata in a video file.  Any video tagging utility can be used as
long as it can be called from the command line.  See the CONFIGURATION
section for details on specifying a tagging tool to Vimeta.  If you
decide to use AtomicParsley it's highly recommended that you use the
fork maintained by `wez` at <https://bitbucket.org/wez/atomicparsley>.

# OPTIONS

The following command-line options are available to all of the
commands listed in the COMMANDS section.

`--verbose`
:    Enable verbose output

`-d`, `--dry-run`
:    Don't actually do anything.  Instead, print out what would be
     done.  Automatically enables `--verbose`.

# COMMANDS

## vimeta \--version

Display the Vimeta version number and then exit.

## vimeta config [*options*]

Create a configuration file for Vimeta.

`-k`, `--key`=*STRING*
:    The API key for TheMovieDB to record into the configuration file.

## vimeta movie [*options*] *FILE*

Look up movie metadata from TheMovieDB and tag a video file.  Vimeta
will interactively prompt for a movie name and provide a list of
matching movies to choose from.

`-i`, `--id`=*ID*
:    Avoid interactive prompting by supplying a movie ID
     assigned by TheMovieDB.com.

## vimeta tv [*options*] [*FILE*]...

Look up TV series metadata from TheMovieDB and tag one or more video
files.  Episode video files can be given on the command line or
specified using a mapping file.  See MAPPING FILES for more
information about creating a file that maps episodes to video files.

Vimeta will interactively prompt for a search string and then list
matching TV series.  The starting season number and starting episode
number should be given on the command line or they will default to 1.
Each video file will be assigned the next episode number in the
series.  Vimeta is smart enough to switch to the next season after the
last episode of the specified season.

`-s`, `--season`=*NUMBER*
:    Specify the starting season number to tag into the episode files.
     The season number will be incremented after the last episode of a
     season is tagged.  If the season number isn't specified it will
     default to 1.

`-e`, `--episode`=*NUMBER*
:    Specify the staring episode number.  If not specified it defaults
     to 1.

`-m`, `--map`=*FILE*
:    Use *FILE* as a mapping file.  The mapping file should contain
     two columns, the first specifies the season and episode numbers
     and the second the file name for the episode video file.  See the
     MAPPING FILE section for more details about mapping files.

`-i`, `--id`=*ID*
:    Avoid interactive prompting by supplying a TV series ID
     assigned by TheMovieDB.com.

# CONFIGURATION

The Vimeta configuration file is a YAML file containing a few keys.
You should begin by creating a default configuration file using the
*config* command.

## Description of Keys

`tmdb_key`
:    The API key issued by TheMovieDB.

`cmd_movie`
:    A format string used to execute a tagging tool for movie files.
     See FORMAT STRINGS for more information.

`cmd_tv`
:    A format string used to execute a tagging tool for TV episode
     files.  See FORMAT STRINGS for more information.

# FORMAT STRINGS

## Description

Vimeta uses format strings to specify the command to run in order to
tag video files, and the arguments needed to pass metadata to the
tagging utility.  These format strings are similar to those used with
the `printf` family of functions and utilities.

In format stings, the percent character (`%`), followed by a single
character, is replaced by one of the metadata attributes.  Percent
characters are removed and do not appear in the output string.
Therefore, if you need a literal percent character to appear in the
output string, you should use two percent characters (`%%`).

Format strings in Vimeta can also contain conditional parts.  These
are introduced with the `%{` format specifier, which must be
terminated with the `%}` format specifier.  The text and any format
specifiers enclosed inside the opening and closing brackets are only
injected into the output string if all of the enclosed format
specifiers have valid values.

As an example, not all movies have release dates.  Supplying a
command-line flag along with a blank release date to a tagging tool
will most likely cause a problem.  It would be best to avoid using the
command-line flag altogether.  The following example does just this:

    somecommand %{ --release-date %Y %}

Here, `%Y` stands in for the full release date.  If it's not present
in the metadata then none of the text between the brackets will be
injected into the output string.  If multiple format specifiers are
used, all of them have to be present in the metadata for any of the
text between the brackets to be included in the output text.

## Format Specifiers for Movies

The following format specifiers are available for tagging movie files.
Not all specifiers are guaranteed to be present.  Consider using
conditional specifiers.

`%Y`
:    Full release date in XML schema notation.

`%a`
:    Path to a temporary file containing a movie poster image file.

`%d`
:    Movie description (summary text, truncated to 255 characters).

`%f`
:    Path to the video file to be tagged.

`%g`
:    Name of first listed genre.

`%t`
:    Movie title.

`%y`
:    Release year.

## Format Specifiers for Episodes

The following format specifiers are available for tagging TV episode
files.  Not all specifiers are guaranteed to be present.  Consider
using conditional specifiers.

`%Y`
:    Full air date in XML schema notation.

`%a`
:    Path to a temporary file containing a season poster image file.

`%d`
:    Episode description (summary text, truncated to 255 characters).

`%e`
:    Episode number.

`%f`
:    Path to the video file to be tagged.

`%n`
:    Name of the TV series.

`%s`
:    Season number.

`%t`
:    Episode name.

`%y`
:    Year that the episode aired.

# MAPPING FILES

Vimeta can use mapping files that specify hints about metadata and
which video files to tag.  For example, the *tv* command can use
mapping files to link video files to specific season and episode
numbers.

Mapping files are plain text files that contain two columns separated
by whitespace.  Comments and blank lines are ignored.  Comments begin
with a pound sign (`#`) and continue to the end of the line.

The first column should contain information relevant to the command
using the mapping file.  For example, the *tv* command expects the
first column to specify the season and episode number.

The second column is always a file name.  If the file doesn't exist
and the file name lacks a file extension, `.m4v` will be added to the
file name automatically.

Example mapping file for the *tv* command:

    # This is a comment.
    S2E3 track03.m4v  # Season 2, Episode 3
    S3E1 track01.m4v  # Season 3, Episode 1

# EXAMPLES

## Tag Several Episode Files

    vimeta tv -s 3 -e 10 file01.m4v file02.m4v

With this command, Vimeta will prompt for the name of the TV series
and then search TheMovieDB.  It will then generate a menu where the
correct TV series can be chosen.  Both files will be tagged as
belonging to season 3.  The first file is episode 10 and the second
file is episode 11.  (For more control over how season and episode
numbers are assigned to files, see the section on MAPPING FILES.)

[TheMovieDB]: http://www.themoviedb.org/
[AtomicParsley]: https://bitbucket.org/wez/atomicparsley
