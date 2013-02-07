{-

This file is part of the Haskell package vimeta. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/vimeta/LICENSE. No part of
vimeta package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

module Vimeta.AtomicParsley where
import System.Cmd (rawSystem)
import System.Exit (ExitCode(..))

type Options = [(String, String)]

update :: FilePath -> Options -> IO ()
update file opts =
  do eCode <- rawSystem ap (file:flatOpts)
     case eCode of
       ExitSuccess   -> return ()
       ExitFailure i -> putStrLn $ ap ++ " failed with: " ++ show i
  where ap       = "AtomicParsley"
        flatOpts = foldr (\(x, y) a -> x:y:a) def opts
        def      = ["--overWrite"]
