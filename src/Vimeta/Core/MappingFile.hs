-- |
--
-- Copyright:
--   This file is part of the package vimeta. It is subject to the
--   license terms in the LICENSE file found in the top-level
--   directory of this distribution and at:
--
--     https://github.com/pjones/vimeta
--
--   No part of this package, including this file, may be copied,
--   modified, propagated, or distributed except according to the terms
--   contained in the LICENSE file.
--
-- License: BSD-2-Clause
--
-- | Mapping files can be used to map file names to other information.
module Vimeta.Core.MappingFile
  ( Parser,
    parseMappingFile,
  )
where

import Data.Char (isSpace)
import qualified Data.Text.IO as Text
import System.Directory (doesFileExist)
import System.FilePath (takeExtension)
import Text.Parsec hiding ((<|>))
import Vimeta.Core.Vimeta

-- | Parser type.
type Parser a = ParsecT Text () Identity a

-- | Internal token used for parsing.
data Token a = Comment | Entry FilePath a

-- | Parse a mapping file.
parseMappingFile ::
  (MonadIO m) =>
  -- | File name for the mapping file.
  FilePath ->
  -- | Parser for the second column.
  Parser a ->
  Vimeta m [(FilePath, a)]
parseMappingFile filename p = do
  contents <- runIO $ Text.readFile filename

  case runIdentity $ runParserT (mapping p) () filename contents of
    Left e -> throwError (show e)
    Right m -> checkFileMappingOrDie m

checkFileMappingOrDie ::
  (MonadIO m) =>
  [(FilePath, a)] ->
  Vimeta m [(FilePath, a)]
checkFileMappingOrDie xs =
  do
    ys <- checkFileMapping xs
    if null (lefts ys)
      then return (rights ys)
      else throwError $ report (lefts ys)
  where
    report :: [(FilePath, a)] -> String
    report fs =
      "the following files are listed in the mapping file "
        ++ "but they don't exist: \n"
        ++ intercalate "\n" (map fst fs)

-- | Checks to see that all of the file names mentioned exist.  If a
-- file doesn't exist the @m4v@ file extension is added to it and the
-- existence checking happens again.
checkFileMapping ::
  (MonadIO m) =>
  -- | The mapping.
  [(FilePath, a)] ->
  Vimeta m [Either (FilePath, a) (FilePath, a)]
checkFileMapping = mapM checkFile
  where
    checkFile ::
      (MonadIO m) =>
      (FilePath, a) ->
      Vimeta m (Either (FilePath, a) (FilePath, a))
    checkFile f@(filename, a) = do
      let ext = takeExtension filename
      exists <- runIO (doesFileExist filename)

      case exists of
        False
          | null ext -> checkFile (filename ++ ".m4v", a)
          | otherwise -> return $ Left f
        True -> return $ Right f

-- | The actual file parser.
mapping :: Parser a -> Parser [(FilePath, a)]
mapping p = entries <$> manyTill (whitespace <|> comment <|> fileName p) eof
  where
    entries :: [Token a] -> [(FilePath, a)]
    entries = concatMap extract . filter predicate
    predicate :: Token a -> Bool
    predicate (Entry _ _) = True
    predicate Comment = False
    extract :: Token a -> [(FilePath, a)]
    extract (Entry f a) = [(f, a)]
    extract Comment = []

-- | Parse a file name followed by whatever the second column parser
-- extracts.
fileName :: Parser a -> Parser (Token a)
fileName p =
  do
    first <- anyChar
    others <- manyTill anyChar (lookAhead space)
    a <- spaceWithoutNewline >> p
    return $ Entry (first : others) a
    <?> "filename and mapping"

-- | Skip whitespace.
whitespace :: Parser (Token a)
whitespace = skipMany1 space >> return Comment

-- | Like whitespace, but doesn't span multiple lines.
spaceWithoutNewline :: Parser ()
spaceWithoutNewline = skipMany1 $ satisfy (\c -> isSpace c && c /= '\n')

-- | Skip comments.
comment :: Parser (Token a)
comment = (char '#' >> manyTill anyChar newline >> return Comment) <?> "comment"
