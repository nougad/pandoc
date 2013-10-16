{-# LANGUAGE RelaxedPolyRec #-} -- needed for inlinesBetween on GHC < 7
{-
Copyright (C) 2006-2010 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Readers.Vimwiki
   Copyright   : Copyright (C) 2006-2013 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of vimwiki-formatted plain text to 'Pandoc' document.
-}
module Text.Pandoc.Readers.Vimwiki ( readVimwiki,
                                     readVimwikiWithWarnings ) where

import Data.Maybe
import Text.Pandoc.Definition
import qualified Data.Text as T
import Data.Text (Text)
import qualified Text.Pandoc.Builder as B
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Builder (Inlines, Blocks, trimInlines, (<>))
import Text.Pandoc.Options
import Text.Pandoc.Shared
import Text.Pandoc.Parsing hiding (tableWith)
import Data.Monoid (mconcat, mempty)
import Control.Applicative ((<$>), (<*), (*>), (<$))
import Control.Monad

type VimwikiParser = Parser [Char] ParserState

-- | Read markdown from an input string and return a Pandoc document.
readVimwiki :: ReaderOptions -- ^ Reader options
            -> String        -- ^ String to parse (assuming @'\n'@ line endings)
            -> Pandoc
readVimwiki opts s =
  (readWith parseVimwiki) def{ stateOptions = opts } (s ++ "\n\n")

-- | Read markdown from an input string and return a pair of a Pandoc document
-- and a list of warnings.
readVimwikiWithWarnings :: ReaderOptions -- ^ Reader options
                        -> String        -- ^ String to parse (assuming @'\n'@ line endings)
                        -> (Pandoc, [String])
readVimwikiWithWarnings opts s =
  (readWith parseVimwikiWithWarnings) def{ stateOptions = opts } (s ++ "\n\n")
 where parseVimwikiWithWarnings = do
         doc <- parseVimwiki
         warnings <- stateWarnings <$> getState
         return (doc, warnings)

parseVimwiki :: VimwikiParser Pandoc
parseVimwiki = do
  blocks <- parseBlocks
  st <- getState
  let Pandoc _ bs = B.doc $ runF blocks st
  return $ Pandoc nullMeta bs

trimInlinesF :: F Inlines -> F Inlines
trimInlinesF = liftM trimInlines

--
-- inline
--

inline :: VimwikiParser (F Inlines)
inline = choice [ whitespace
                , str
                , endline
                ]

str :: VimwikiParser (F Inlines)
str = undefined -- TODO

whitespace :: VimwikiParser (F Inlines)
whitespace = spaceChar >> return <$> (lb <|> regsp) <?> "whitespace"
  where lb = spaceChar >> skipMany spaceChar >> option B.space (endline >> return B.linebreak)
        regsp = skipMany spaceChar >> return B.space

endline :: VimwikiParser (F Inlines)
endline = do
  newline
  notFollowedBy blankline
  return undefined -- TODO

--
-- block
--

parseBlocks :: VimwikiParser (F Blocks)
parseBlocks = mconcat <$> manyTill block eof

block :: VimwikiParser (F Blocks)
block = choice [ mempty <$ blanklines
               , header
               ] <?> "block"


header :: VimwikiParser (F Blocks)
header = atxHeader <?> "header"

atxHeader :: VimwikiParser (F Blocks)
atxHeader = do
  level <- many1 (char '=') >>= return . length
  skipSpaces
  text <- trimInlinesF . mconcat <$> endBy1 inline (atxClosing level)
  return $ B.headerWith nullAttr level <$> text

atxClosing :: Int -> VimwikiParser ()
atxClosing level = do
  count level (char '=')
  skipSpaces
  blanklines
  return ()

