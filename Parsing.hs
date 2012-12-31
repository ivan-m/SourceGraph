{-
Copyright (C) 2009 Ivan Lazar Miljenovic <Ivan.Miljenovic@gmail.com>

This file is part of SourceGraph.

SourceGraph is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
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
   Module      : Parsing
   Description : Parse the given Haskell modules.
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : GPL-3 or later.
   Maintainer  : Ivan.Miljenovic@gmail.com

   Parse the given Haskell modules.
 -}
module Parsing
    ( FileContents
    , ParsedModules
    , ModName
    , createModule
    , parseHaskell
    -- from Parsing.Types
    , moduleSep
    ) where

import Parsing.Types
import Parsing.ParseModule

import Language.Haskell.Exts(parseFileContentsWithMode)
import Language.Haskell.Exts.Parser( ParseMode(..)
                                   , ParseResult(..)
                                   , defaultParseMode)
import Language.Haskell.Exts.Syntax(Module)

import Data.Either(partitionEithers)

type FileContents = (FilePath,String)

-- | Parse all the files and return the map.
--   This uses laziness to evaluate the 'HaskellModules' result
--   whilst also using it to parse all the modules to create it.
parseHaskell    :: [FileContents] -> ([FilePath],ParsedModules)
parseHaskell fc = (failed,hms)
    where
      (failed,ms) = parseFiles fc
      hms = createModuleMap hss
      hss = map (parseModule hms) ms

-- | Attempt to parse an individual file.
parseFile       :: FileContents -> Either FilePath Module
parseFile (p,f) = case (parseFileContentsWithMode mode f) of
                    (ParseOk hs) -> Right hs
                    _            -> Left p
    where
      mode = defaultParseMode { parseFilename = p
                              , fixities = Nothing
                              }

-- | Parse all the files that you can.
parseFiles :: [FileContents] -> ([FilePath],[Module])
parseFiles = partitionEithers . map parseFile
