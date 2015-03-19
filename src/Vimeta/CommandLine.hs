{-

This file is part of the Haskell package vimeta. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/vimeta/LICENSE. No part of
vimeta package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Vimeta.CommandLine (parseAndDispatch) where

--------------------------------------------------------------------------------
import Data.Monoid
import Options.Applicative
import qualified Vimeta.Movie as VM
import qualified Vimeta.TV    as VT

--------------------------------------------------------------------------------
-- Possible command line configurations.
data CommandLine = MovieCmd VM.Config
                 | TVCmd VT.Config
                 deriving (Show)

--------------------------------------------------------------------------------
-- Parse the configuration for a movie.
movie :: Parser VM.Config
movie = VM.Config
        <$> option auto (short 'i' <> long "movie-id" <> metavar "ID" <>
                    help "Movie ID assigned by TheMovieDB.org")
        <*> argument str (metavar "FILE")

--------------------------------------------------------------------------------
-- Parse the configuration for a TV series.
tv :: Parser VT.Config
tv = VT.Config
     <$> option auto (short 'i' <> long "series" <> metavar "ID" <>
                 help "TV series ID assigned by TheTVDB.com")
     <*> option auto (short 's' <> long "season" <> metavar "NUM" <>
                 help "Which season number to store in the episodes")
     <*> optional (option auto (short 'e' <> long "episode" <> metavar "NUM" <>
                           help "The starting episode number to use"))
     <*> some (argument str (metavar "FILE [FILE...]"))

--------------------------------------------------------------------------------
-- Parse all the possible sub-commands.
config :: Parser CommandLine
config = subparser
         (  command "movie" (info (MovieCmd <$> movie) (progDesc movieDesc))
         <> command "tv"    (info (TVCmd <$> tv)       (progDesc tvDesc))
         )
  where movieDesc = "Set movie metadata using TheMovieDB"
        tvDesc    = "Set TV episode metadata using TheTVDB"

--------------------------------------------------------------------------------
-- Dispatch to the correct sub-command.
dispatch :: CommandLine -> IO ()
dispatch (MovieCmd c) = VM.update c
dispatch (TVCmd c)    = VT.update c

--------------------------------------------------------------------------------
-- Actually parse the command line and delegate to the dispatcher.
parseAndDispatch :: IO ()
parseAndDispatch = execParser opts >>= dispatch
  where opts = info (config <**> helper) idm
