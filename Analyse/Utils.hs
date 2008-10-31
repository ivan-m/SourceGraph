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
   Module      : Analyse.Utils
   Description : Utility functions and types for analysis.
   Copyright   : (c) Ivan Lazar Miljenovic 2008
   License     : GPL-3 or later.
   Maintainer  : Ivan.Miljenovic@gmail.com

   Utility functions and types for analysis.
 -}
module Analyse.Utils where

import Data.Graph.Analysis
import Data.GraphViz
import Data.Graph.Inductive hiding (graphviz)


-- | Defining a wrapper around String to define a sensible 'show' definition.
newtype AString = AS String
    deriving (Eq, Ord)

instance Show AString where
    show (AS f) = f

-- | Create a graph in the 'DocGraph' format.
--   Takes in the filepath, title and the graph to be drawn.
toGraph       :: (Show a) => FilePath -> String -> AGr a -> DocGraph
toGraph p t g = (p,Text t,dg)
    where
      dg = graphviz t g

toClusters       :: (Show c, ClusterLabel a c) => FilePath -> String
                 -> AGr a -> DocGraph
toClusters p t g = (p, Text t, dg)
    where
      dg = graphvizClusters t g

-- | Cyclomatic complexity
cyclomaticComplexity    :: GraphData a -> Int
cyclomaticComplexity gd = e - n + 2*p
    where
      p = length $ applyAlg componentsOf gd
      n = applyAlg noNodes gd
      e = length $ applyAlg labEdges gd
