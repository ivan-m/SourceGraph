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
   Module      : Analyse.Utils
   Description : Utility functions and types for analysis.
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : GPL-3 or later.
   Maintainer  : Ivan.Miljenovic@gmail.com

   Utility functions and types for analysis.
 -}
module Analyse.Utils where

import Data.Graph.Analysis hiding (Bold)

import Data.List(groupBy, sortBy)
import Data.Function(on)
import qualified Data.Set as S
import Data.Set(Set)

-- -----------------------------------------------------------------------------

-- | Cyclomatic complexity
cyclomaticComplexity    :: GraphData a b -> Int
cyclomaticComplexity gd = e - n + 2*p
    where
      p = length $ applyAlg componentsOf gd
      n = applyAlg noNodes gd
      e = length $ applyAlg labEdges gd

groupSortBy   :: (Ord b) => (a -> b) -> [a] -> [[a]]
groupSortBy f = groupBy ((==) `on` f) . sortBy (compare `on` f)


bool       :: a -> a -> Bool -> a
bool f t b = if b then t else f

chainEdges'    :: NGroup -> Set Edge
chainEdges' [] = S.empty
chainEdges' ns = S.fromList $ zip ns (tail ns)

chainEdges :: [NGroup] -> Set Edge
chainEdges = S.unions . map chainEdges'

cycleEdges'    :: NGroup -> Set Edge
cycleEdges' []         = S.empty
cycleEdges' [_]        = S.empty
cycleEdges' ns@(n:ns') = (n, last ns') `S.insert` chainEdges' ns

cycleEdges :: [NGroup] -> Set Edge
cycleEdges = S.unions . map cycleEdges'

cliqueEdges'    :: NGroup -> Set Edge
cliqueEdges' ns = S.fromList [(f,t) | f <- ns, t <- ns, f /= t]

cliqueEdges :: [NGroup] -> Set Edge
cliqueEdges = S.unions . map cliqueEdges'

-- -----------------------------------------------------------------------------

-- For core

coreDesc   :: String -> DocElement
coreDesc w = Paragraph [ Text "The core of"
                       , BlankSpace
                       , Text w
                       , BlankSpace
                       , Text "is calculated by recursively removing roots\
                               \ and leaves of the call graph; as such, it\
                               \ can be considered as the section where all\
                               \ the \"real work\" is done."
                       ]
