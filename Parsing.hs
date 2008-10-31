{-
Copyright (C) 2008 Ivan Lazar Miljenovic <Ivan.Miljenovic@gmail.com>

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
   Copyright   : (c) Ivan Lazar Miljenovic 2008
   License     : GPL-3 or later.
   Maintainer  : Ivan.Miljenovic@gmail.com

   Parse the given Haskell modules.
 -}
module Parsing
    ( FileContents
    , HaskellModules
    , ModuleName
    , createModule
    , parseHaskell
    -- from Parsing.Types
    , moduleSep
    ) where

import Parsing.Types
import Parsing.ParseModule

import Language.Haskell.Exts.Parser hiding (parseModule)
import Language.Haskell.Exts.Syntax(HsModule)

import Data.Maybe

type FileContents = (FilePath,String)

-- | Parse all the files and return the map.
--   This uses laziness to evaluate the 'HaskellModules' result
--   whilst also using it to parse all the modules to create it.
parseHaskell    :: [FileContents] -> HaskellModules
parseHaskell fc = hms
    where
      ms = parseFiles fc
      hms = createModuleMap hss
      hss = map (parseModule hms) ms

-- | Attempt to parse an individual file.
parseFile       :: FileContents -> Maybe HsModule
parseFile (p,f) = case (parseModuleWithMode mode f) of
                    (ParseOk hs) -> Just hs
                    _            -> Nothing
    where
      mode = ParseMode p

-- | Parse all the files that you can.
parseFiles :: [FileContents] -> [HsModule]
parseFiles = catMaybes . map parseFile
