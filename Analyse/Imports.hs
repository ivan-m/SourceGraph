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
   Module      : Analyse.Imports
   Description : Analyse module imports.
   Copyright   : (c) Ivan Lazar Miljenovic 2008
   License     : GPL-3 or later.
   Maintainer  : Ivan.Miljenovic@gmail.com

   Analysis of Haskell module importing.
 -}
module Analyse.Imports (analyseImports) where

import Parsing.Types
import Analyse.Utils

import Data.Graph.Analysis

import Data.Maybe
import Text.Printf
import System.Random

type ImportData = GraphData ModuleName

-- | Analyse the imports present in the software.  Takes in a random seed
--   as well as a list of all modules exported.
analyseImports :: (RandomGen g) => g -> [ModuleName] -> HaskellModules
               -> DocElement
analyseImports g exps hm = Section title elems
    where
      id = importsToGraph exps hm
      title = Text "Analysis of module imports"
      elems = catMaybes
              $ map ($id) [ graphOf
                          , clustersOf g
                          , cycleCompAnal
                          , rootAnal
                          , componentAnal
                          , cycleAnal
                          , chainAnal
                          ]

importsToGraph          :: [ModuleName] -> HaskellModules -> ImportData
importsToGraph exps hms = importData params
    where
      params = Params { dataPoints    = modulesIn hms
                      , relationships = moduleImports hms
                      , roots         = exps
                      , directed      = True
                      }

graphOf    :: ImportData -> Maybe DocElement
graphOf id = Just $ Section title [gi]
    where
      title = Text "Visualisation of imports"
      gi = GraphImage $ applyAlg dg id
      dg g = toGraph "imports" label g
      label = "Import visualisation"

clustersOf      :: (RandomGen g) => g -> ImportData -> Maybe DocElement
clustersOf g id = Just $ Section title [text, gi, textAfter, cw, rng]
    where
      title = Text "Visualisation of module groupings"
      gi = GraphImage $ applyAlg dg id
      text = Paragraph [Text "Here is the current module groupings:"]
      dg gr = toClusters "importCluster" label gr
      label = "Module groupings"
      textAfter = Paragraph [Text "Here are two proposed module groupings:"]
      cw = GraphImage
           . toClusters "importCW" "Chinese Whispers module groupings"
           $ applyAlg (chineseWhispers g) id
      rng = GraphImage
            . toClusters "importRNG" "Relative Neighbourhood module groupings"
            $ applyAlg relativeNeighbourhood id

componentAnal :: ImportData -> Maybe DocElement
componentAnal id
    | single comp = Nothing
    | otherwise   = Just elem
    where
      comp = applyAlg componentsOf id
      len = length comp
      elem = Section title [Paragraph [Text text]]
      title = Text "Import component analysis"
      text = printf "The imports have %d components.  \
                     \You may wish to consider splitting the code up." len

cycleAnal :: ImportData -> Maybe DocElement
cycleAnal id
    | null cycs = Nothing
    | otherwise = Just elem
    where
      cycs = applyAlg cyclesIn id
      cycs' = Paragraph $ map (Text . showCycle) cycs
      text = Text "The imports have the following cycles:"
      textAfter = Text "Whilst this is valid, it may make it difficult \
                       \to use in ghci, etc."
      elem = Section title [Paragraph [text], cycs', Paragraph [textAfter]]
      title = Text "Cycle analysis of imports"

chainAnal :: ImportData -> Maybe DocElement
chainAnal id
    | null chns = Nothing
    | otherwise = Just elem
    where
      chns = applyAlg chainsIn id
      chns' = Paragraph $ map (Text . showPath) chns
      text = Text "The imports have the following chains:"
      textAfter = Text "These chains can all be compressed down to \
                       \a single module."
      elem = Section title [Paragraph [text], chns', Paragraph [textAfter]]
      title = Text "Import chain analysis"

rootAnal :: ImportData -> Maybe DocElement
rootAnal id
    | asExpected = Nothing
    | otherwise  = Just elem
    where
      (wntd, ntRs, ntWd) = classifyRoots id
      asExpected = (null ntRs) && (null ntWd)
      rpt (s,ns) = if (null ns)
                   then Nothing
                   else Just [ Paragraph
                               [Text
                                $ concat ["These modules are those that are "
                                         , s, ":"]]
                             , Paragraph [Text $ showNodes ns]]
      ps = concat . catMaybes
           $ map rpt [ ("in the export list and roots",wntd)
                     , ("in the export list but not roots",ntWd)
                     , ("not in the export list but roots",ntRs)]
      elem = Section title ps
      title = Text "Import root analysis"

cycleCompAnal    :: ImportData -> Maybe DocElement
cycleCompAnal id = Just $ Section title [par]
    where
      cc = cyclomaticComplexity id
      title = Text "Cyclomatic Complexity of imports"
      par = Paragraph [text, textAfter, link]
      text = Text
             $ printf "The cyclomatic complexity of the imports is: %d" cc
      textAfter = Text "For more information on cyclomatic complexity, \
                       \please see:"
      link = DocLink (Text "Wikipedia: Cyclomatic Complexity")
                     (URL "http://en.wikipedia.org/wiki/Cyclomatic_complexity")
