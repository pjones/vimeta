{-

This file is part of the Haskell package vimeta. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/vimeta/LICENSE. No part of
vimeta package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

module Vimeta.CommandLine (parseAndDispatch) where
import qualified Vimeta.Movie as VM
import qualified Vimeta.TV    as VT
import Options.Applicative
import Data.Monoid

-- Possible command line configurations.
data CommandLine = MovieCmd VM.Config
                 | TVCmd VT.Config
                 deriving (Show)

-- Parse the configuration for a movie.
movie :: Parser VM.Config
movie = VM.Config
        <$> option (short 'i' <> long "movie-id" <> metavar "ID" <>
                    help "Movie ID assigned by TheMovieDB.org")
        <*> argument str (metavar "FILE")

-- Parse the configuration for a TV series.
tv :: Parser VT.Config
tv = VT.Config
     <$> option (short 'i' <> long "series" <> metavar "ID" <>
                 help "TV series ID assigned by TheTVDB.com")
     <*> option (short 's' <> long "season" <> metavar "NUM" <>
                 help "Which season number to store in the episodes")
     <*> optional (option (short 'e' <> long "episode" <> metavar "NUM" <>
                           help "The starting episode number to use"))
     <*> arguments1 str (metavar "FILE [FILE...]")

-- Parse all the possible sub-commands.
config :: Parser CommandLine
config = subparser
         (  command "movie" (info (MovieCmd <$> movie) (progDesc movieDesc))
         <> command "tv"    (info (TVCmd <$> tv)       (progDesc tvDesc))
         )
  where movieDesc = "Set movie metadata using TheMovieDB"
        tvDesc    = "Set TV episode metadata using TheTVDB"

-- Dispatch to the correct sub-command.
dispatch :: CommandLine -> IO ()
dispatch (MovieCmd c) = VM.update c
dispatch (TVCmd c)    = putStrLn $ "TV: " ++ show c

-- Actually parse the command line and delegate to the dispatcher.
parseAndDispatch :: IO ()
parseAndDispatch = execParser opts >>= dispatch
  where opts = info (config <**> helper) idm
