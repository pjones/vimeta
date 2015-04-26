{-

This file is part of the vimeta package. It is subject to the license
terms in the LICENSE file found in the top-level directory of this
distribution and at git://pmade.com/vimeta/LICENSE. No part of the
vimeta package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Vimeta.UI.CommandLine (run) where

--------------------------------------------------------------------------------
import Data.Monoid
import qualified Data.Text as Text
import Options.Applicative
import System.Console.Byline
import System.Exit
import qualified Vimeta.UI.CommandLine.Config as Config

--------------------------------------------------------------------------------
data Command = CmdConfig Config.Options

--------------------------------------------------------------------------------
optionsParser :: Parser Command
optionsParser = subparser $ mconcat [config]
  where
    config = command "config"
             (info (CmdConfig <$> Config.optionsParser) (progDesc configDesc))

    configDesc = "Create a new configuration file"

--------------------------------------------------------------------------------
run :: IO ()
run = do
  options <- execParser $ info (optionsParser <**> helper) idm
  result  <- case options of
               CmdConfig o -> Config.run o

  case result of
    Right _ -> exitSuccess
    Left e  -> do runByline (reportLn Error $ text $ Text.pack e)
                  exitFailure
