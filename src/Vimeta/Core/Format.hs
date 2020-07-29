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
module Vimeta.Core.Format
  ( FormatTable,
    fromFormatString,
    formatYear,
    formatFullDate,
  )
where

import Data.Time (Day (..), defaultTimeLocale, formatTime)
import Relude.Extra.Map
import System.Process.Internals (translate)
import Text.Parsec hiding ((<|>))

-- | Mapping of format characters to their possible replacement text.
type FormatTable = Map Char (Maybe Text)

-- | Syntax tree for format strings.
data Replacement
  = -- | Replace the given character.
    Replace Char
  | -- | Conditional section.
    Condition [(Text, Replacement)]
  | -- | End of input (or condition).
    EndOfInput

-- | Parser type.
type Parser a = ParsecT Text () (Reader FormatTable) a

-- | Replace format characters prefixed with a @%@ with the
-- replacement text found in the given 'Map'.
fromFormatString ::
  -- | Format character mapping.
  FormatTable ->
  -- | Name of format string.
  String ->
  -- | Input text.
  Text ->
  -- | Output text or error.
  Either String Text
fromFormatString table name input =
  case runReader (runParserT parseFormatString () name input) table of
    Left e -> Left (show e)
    Right t -> Right t

-- | Format a 'Day' using the XML schema notation.
formatFullDate :: Maybe Day -> Maybe Text
formatFullDate = formatDay "%Y-%m-%dT00:00:00Z"

-- | Format a 'Day' displaying just the year.
formatYear :: Maybe Day -> Maybe Text
formatYear = formatDay "%Y"

formatDay :: String -> Maybe Day -> Maybe Text
formatDay fmt d = toText . formatTime defaultTimeLocale fmt <$> d

parseFormatString :: Parser Text
parseFormatString = manyTill go eof >>= renderFormatString
  where
    go = findFormatCharacter >>= mkReplacement

-- | Render a format string syntax table as a 'Text' value.
renderFormatString :: [(Text, Replacement)] -> Parser Text
renderFormatString rs = do
  table <- ask
  return (mconcat $ map (render table) rs)
  where
    escape :: Text -> Text
    escape = toText . translate . toString
    findChar :: FormatTable -> Char -> Text
    findChar t c = fromMaybe "" $ join (lookup c t)
    render :: FormatTable -> (Text, Replacement) -> Text
    render tbl (txt, Replace c) = txt <> escape (findChar tbl c)
    render tbl (txt, Condition c) = txt <> renderCondition tbl c
    render _ (txt, EndOfInput) = txt
    renderCondition :: FormatTable -> [(Text, Replacement)] -> Text
    renderCondition tbl conds =
      if all (checkCondition tbl) conds
        then mconcat $ map (render tbl) conds
        else mempty
    checkCondition :: FormatTable -> (Text, Replacement) -> Bool
    checkCondition tbl (_, Replace c) = isJust (join $ lookup c tbl)
    checkCondition tbl (_, Condition c) = all (checkCondition tbl) c
    checkCondition _ (_, EndOfInput) = True

-- | Location a format character preceded by a @'%'@ character.
-- Returns the text leading up to the format character and the
-- character itself.
findFormatCharacter :: Parser (Text, Maybe Char)
findFormatCharacter = do
  beforeText <- toText <$> manyTill anyChar (try eofOrFormatChar)
  formatChar <- try $ (Just <$> anyChar) <|> return Nothing
  return (beforeText, formatChar)
  where
    eofOrFormatChar :: Parser ()
    eofOrFormatChar = eof <|> void (char '%')

-- | Translate the output from 'findFormatCharacter' into a syntax node.
mkReplacement :: (Text, Maybe Char) -> Parser (Text, Replacement)
mkReplacement (beforeText, formatChar) =
  case formatChar of
    Nothing -> return (beforeText, EndOfInput)
    Just '{' -> (beforeText,) <$> (Condition <$> parseConditional)
    Just c -> return (beforeText, Replace c)

-- | Parse a conditional section out of a format string.
parseConditional :: Parser [(Text, Replacement)]
parseConditional = do
  (beforeText, formatChar) <- findFormatCharacter

  case formatChar of
    -- Reached the end of the format string.
    Nothing -> unexpected "end of format string, expected `%}'"
    -- Start another conditional.
    Just '{' -> do
      other <- parseConditional
      return [(beforeText, Condition other)]

    -- End this conditional.
    Just '}' -> return [(beforeText, EndOfInput)]
    -- Add this replacement to the list, fetch the next one.
    Just c -> do
      next <- parseConditional
      return ((beforeText, Replace c) : next)
