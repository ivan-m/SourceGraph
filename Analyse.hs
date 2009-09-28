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
   Module      : Analyse
   Description : Analyse Haskell software
   Copyright   : (c) Ivan Lazar Miljenovic 2008
   License     : GPL-3 or later.
   Maintainer  : Ivan.Miljenovic@gmail.com

   Analyse Haskell software
 -}
module Analyse where

import Analyse.Module
import Analyse.Imports
import Analyse.Everything
import Parsing.Types

import Data.Graph.Analysis

import System.Random(RandomGen)

-- | Analyse an entire Haskell project.  Takes in a random seed,
--   the list of exported modules and the parsed Haskell code in
--   'HaskellModules' form.
analyse            :: (RandomGen g) => g -> [ModName] -> ParsedModules
                   -> [DocElement]
analyse g exps hms = [ analyseModules hms
                     , analyseImports exps hms
                     , analyseEverything g exps hms
                     ]
