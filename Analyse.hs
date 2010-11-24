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
   Module      : Analyse
   Description : Analyse Haskell software
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : GPL-3 or later.
   Maintainer  : Ivan.Miljenovic@gmail.com

   Analyse Haskell software
 -}
module Analyse(analyse, sgLegend) where

import Analyse.Module
import Analyse.Imports
import Analyse.Everything
import Analyse.Colors
import Parsing.Types

import Data.Graph.Analysis hiding (Bold)
import qualified Data.Graph.Analysis.Reporting as R (DocInline(Bold))
import Data.GraphViz.Types
import Data.GraphViz.Attributes

import System.Random(RandomGen)
import Control.Arrow(first)

-- | Analyse an entire Haskell project.  Takes in a random seed,
--   the list of exported modules and the parsed Haskell code in
--   'HaskellModules' form.
analyse            :: (RandomGen g) => g -> [ModName] -> ParsedModules
                   -> [DocElement]
analyse g exps hms = [ analyseEverything g exps hms
                     , analyseImports exps hms
                     , analyseModules hms
                     ]

sgLegend :: [(Either DocGraph DocInline, DocInline)]
sgLegend = map (first Left) [ esCall
                            , mods
                            , esLoc
                            , esData
                            , esClass
                            , esExp
                            , callCats
                            ]

esCall, mods, esLoc, esData, esClass, esExp, callCats :: (DocGraph, DocInline)

esCall = (dg', R.Bold txt)
    where
      dg' = DG "legend_call" (Text "Function Call") dg
      dg = mkLegendGraph ns es
      nAs = [Shape BoxShape, FillColor defaultNodeColor]
      ns = [ (1, Label (StrLabel e1) : nAs)
           , (2, Label (StrLabel e2) : nAs)
           ]
      e1 = "f"
      e2 = "g"
      eAs = [Color [X11Color Black]]
      es = [(1,2,eAs)]
      txt = Grouping [ Text "Two normal functions with"
                     , Emphasis $ Text e1
                     , Text "calling"
                     , Emphasis $ Text e2
                     , Text "."
                     ]

mods = (dg', R.Bold txt)
    where
      dg' = DG "legend_mods" (Text "Module Import") dg
      dg = mkLegendGraph ns es
      nAs = [Shape Tab, FillColor defaultNodeColor]
      ns = [ (1, Label (StrLabel m1) : nAs)
           , (2, Label (StrLabel m2) : nAs)
           ]
      m1 = "Foo"
      m2 = "Bar"
      es = [(1,2,[])]
      txt = Grouping [ Text "Two modules with module"
                     , Emphasis $ Text m1
                     , Text "importing"
                     , Emphasis $ Text m2
                     , Text "."
                     ]

esLoc = (dg', R.Bold $ Text "Entities from different modules.")
    where
      dg' = DG "legend_loc" (Text "From module") dg
      dg = mkLegendGraph ns es
      nAs = [FillColor defaultNodeColor]
      ns = [ (1, Label (StrLabel "Current module")
                   : Style [SItem Bold []] : nAs)
           , (2, Label (StrLabel "Other project module")
                   : Style [SItem Solid []] : nAs)
           , (3, Label (StrLabel "Known external module")
                   : Style [SItem Dashed []] : nAs)
           , (4, Label (StrLabel "Unknown external module")
                   : Style [SItem Dotted []] : nAs)
           ]
      es = []

esData = (dg', R.Bold $ Text "Data type declaration.")
    where
      dg' = DG "legend_data" (Text "Data type declaration") dg
      dg = mkLegendGraph ns es
      nAs = [FillColor defaultNodeColor]
      ns = [ (1, Label (StrLabel "Constructor")
                   : Shape Box3D : nAs)
           , (2, Label (StrLabel "Record function")
                   : Shape Component : nAs)
           ]
      es = [(2,1,[ Color [X11Color Magenta], ArrowTail oDot, ArrowHead vee])]

esClass = (dg', R.Bold $ Text "Class and instance declarations.")
    where
      dg' = DG "legend_class" (Text "Class declaration") dg
      dg = mkLegendGraph ns es
      nAs = [FillColor defaultNodeColor]
      ns = [ (1, Label (StrLabel "Default instance")
                   : Shape Octagon : nAs)
           , (2, Label (StrLabel "Class function")
                   : Shape DoubleOctagon : nAs)
           , (3, Label (StrLabel "Instance for data type")
                   : Shape Octagon : nAs)
           ]
      eAs = [Dir NoDir]
      es = [ (2,1, Color [X11Color Navy] : eAs)
           , (2,3, Color [X11Color Turquoise] : eAs)
           ]

esExp = (dg', R.Bold $ Text "Entity location/accessibility classification.")
    where
      dg' = DG "legend_loc2" (Text "Entity Location") dg
      dg = mkLegendGraph ns es
      ns = zip [1..]
           [ [ Label (StrLabel "Inaccessible entity")
             , FillColor inaccessibleColor]
           , [ Label (StrLabel "Exported root entity")
             , FillColor exportedRootColor]
           , [ Label (StrLabel "Exported non-root entity")
             , FillColor exportedInnerColor]
           , [ Label (StrLabel "Implicitly exported entity")
             , FillColor implicitExportColor]
           , [ Label (StrLabel "Leaf entity")
             , FillColor leafColor]
           , [ Label (StrLabel "Normal entity")
             , FillColor defaultNodeColor]
           ]
      es = []

callCats = (dg', R.Bold $ Text "Edge classification.")
  where
    dg' = DG "legend_edges" (Text "Edge Classification") dg
    dg = mkLegendGraph ns es
    ns = map (flip (,) [Label (StrLabel ""), Shape PointShape]) [1..5]
    eA = Dir NoDir
    es = [ (1,2, [eA, Label (StrLabel "Clique"), Color [cliqueColor]])
         , (2,3, [eA, Label (StrLabel "Cycle"), Color [cycleColor]])
         , (3,4, [eA, Label (StrLabel "Chain"), Color [chainColor]])
         , (4,5, [eA, Label (StrLabel "Normal"), Color [defaultEdgeColor]])
         ]


mkLegendGraph       :: [(Int,Attributes)] -> [(Int,Int,Attributes)]
                       -> DotGraph Node
mkLegendGraph ns es = DotGraph { strictGraph   = False
                               , directedGraph = True
                               , graphID       = Nothing
                               , graphStatements
                                   = DotStmts { attrStmts = atts
                                              , subGraphs = [nSG]
                                              , nodeStmts = []
                                              , edgeStmts = map mkE es
                                              }
                               }
    where
      atts = [ GraphAttrs [OutputOrder EdgesFirst]
             , NodeAttrs [FontSize 10, Style [SItem Filled []]]
             ]
      sgAtts = [GraphAttrs [Rank MinRank]]
      nSG = DotSG { isCluster     = False
                  , subGraphID    = Nothing
                  , subGraphStmts = DotStmts { attrStmts = sgAtts
                                             , subGraphs = []
                                             , nodeStmts = map mkN ns
                                             , edgeStmts = []
                                             }
                  }
      mkN (n,as)   = DotNode n as
      mkE (f,t,as) = DotEdge f t True as
