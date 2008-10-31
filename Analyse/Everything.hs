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
   Module      : Analyse.Software
   Description : Analyse Haskell software
   Copyright   : (c) Ivan Lazar Miljenovic 2008
   License     : GPL-3 or later.
   Maintainer  : Ivan.Miljenovic@gmail.com

   Analysis of the entire overall piece of software.
 -}
module Analyse.Everything where

import Parsing.Types
import Analyse.Utils

import Data.Graph.Analysis

import Data.Maybe
import Text.Printf
import System.Random

type CodeData = GraphData Function


-- | Performs analysis of the entire codebase.
analyseEverything :: (RandomGen g) => g -> [ModuleName] -> HaskellModules
                  -> DocElement
analyseEverything g exps hm = Section sec elems
    where
      cd = codeToGraph exps hm
      sec = Text "Analysis of the entire codebase"
      elems = catMaybes
              $ map ($cd) [ graphOf
                          , clustersOf g
                          , collapseAnal
                          , coreAnal
                          , cycleCompAnal
                          , rootAnal
                          , componentAnal
                          , cliqueAnal
                          , cycleAnal
                          , chainAnal
                          ]


codeToGraph          :: [ModuleName] -> HaskellModules -> CodeData
codeToGraph exps hms = importData params
    where
      exps' = concat . catMaybes $ map (fmap exports . getModule hms) exps
      fl = combineCalls .map functions $ hModulesIn hms
      params = Params { dataPoints    = functionsIn fl
                      , relationships = functionEdges fl
                      , roots         = exps'
                      , directed      = True
                      }

graphOf    :: CodeData -> Maybe DocElement
graphOf cd = Just $ Section sec [gc]
    where
      sec = Text "Visualisation of the entire software"
      gc = GraphImage $ applyAlg dg cd
      dg g = toGraph "code" lbl g
      lbl = "Software visualisation"

clustersOf      :: (RandomGen g) => g -> CodeData -> Maybe DocElement
clustersOf g cd = Just $ Section sec [text, gc, textAfter, cw, blank, rng]
    where
      blank = Paragraph [BlankSpace]
      sec = Text "Visualisation of overall function calls"
      gc = GraphImage $ applyAlg dg cd
      text = Paragraph
             [Text "Here is the current module grouping of functions:"]
      dg gr = toClusters "codeCluster" lbl gr
      lbl = "Module groupings"
      textAfter = Paragraph [Text "Here are two proposed module groupings:"]
      cw = GraphImage
           . toClusters "codeCW" "Chinese Whispers module suggestions"
           $ applyAlg (chineseWhispers g) cd
      rng = GraphImage
            . toClusters "codeRNG" "Relative Neighbourhood module suggestions"
            $ applyAlg relativeNeighbourhood cd

componentAnal :: CodeData -> Maybe DocElement
componentAnal cd
    | single comp = Nothing
    | otherwise   = Just $ Section sec [Paragraph [Text text]]
    where
      comp = applyAlg componentsOf cd
      len = length comp
      sec = Text "Function component analysis"
      text = printf "The functions are split up into %d components.  \
                     \You may wish to consider splitting the code up \
                     \into multiple libraries." len

cliqueAnal :: CodeData -> Maybe DocElement
cliqueAnal cd
    | null clqs = Nothing
    | otherwise = Just el
    where
      clqs = applyAlg cliquesIn cd
      clqs' = return . Enumeration
              $ map (Paragraph . return . Text . showNodes) clqs
      text = Text "The code has the following cliques:"
      el = Section sec $ (Paragraph [text]) : clqs'
      sec = Text "Overall clique analysis"

cycleAnal :: CodeData -> Maybe DocElement
cycleAnal cd
    | null cycs = Nothing
    | otherwise = Just el
    where
      cycs = applyAlg uniqueCycles cd
      cycs' = return . Enumeration
              $ map (Paragraph . return . Text . showCycle) cycs
      text = Text "The code has the following non-clique cycles:"
      el = Section sec $ (Paragraph [text]) : cycs'
      sec = Text "Overall cycle analysis"

chainAnal :: CodeData -> Maybe DocElement
chainAnal cd
    | null chns = Nothing
    | otherwise = Just el
    where
      chns = interiorChains cd
      chns' = return . Enumeration
              $ map (Paragraph . return . Text . showPath) chns
      text = Text "The functions have the following chains:"
      textAfter = Text "These chains can all be compressed down to \
                       \a single function."
      el = Section sec $
           [Paragraph [text]] ++ chns' ++ [Paragraph [textAfter]]
      sec = Text "Overall chain analysis"

rootAnal :: CodeData -> Maybe DocElement
rootAnal cd
    | asExpected = Nothing
    | otherwise  = Just $ Section sec ps
    where
      (wntd, ntRs, ntWd) = classifyRoots cd
      asExpected = (null ntRs) && (null ntWd)
      rpt (s,ns) = if (null ns)
                   then Nothing
                   else Just [ Paragraph
                               [Text
                                $ concat ["These functions are those that are "
                                         , s, ":"]]
                             , Paragraph [Text $ showNodes ns]]
      ps = concat . catMaybes
           $ map rpt [ ("available for use and roots",wntd)
                     , ("available for use but not roots",ntWd)
                     , ("not available for use but roots",ntRs)]
      sec = Text "Import root analysis"


cycleCompAnal    :: CodeData -> Maybe DocElement
cycleCompAnal cd = Just $ Section sec pars
    where
      cc = cyclomaticComplexity cd
      sec = Text "Overall Cyclomatic Complexity"
      pars = [Paragraph [text], Paragraph [textAfter, link]]
      text = Text
             $ printf "The overall cyclomatic complexity is: %d" cc
      textAfter = Text "For more information on cyclomatic complexity, \
                       \please see: "
      link = DocLink (Text "Wikipedia: Cyclomatic Complexity")
                     (URL "http://en.wikipedia.org/wiki/Cyclomatic_complexity")


coreAnal    :: CodeData -> Maybe DocElement
coreAnal cd = if (isEmpty core)
              then Nothing
              else Just $ Section sec [hdr, anal]
    where
      core = applyAlg coreOf cd
      p = "codeCore"
      lbl = "Overall core"
      hdr = Paragraph [Text "The core of software can be thought of as \
                             \the part where all the work is actually done."]
      anal = GraphImage (toGraph p lbl core)
      sec = Text "Overall Core analysis"


collapseAnal    :: CodeData -> Maybe DocElement
collapseAnal cd = if (trivialCollapse gc)
                  then Nothing
                  else Just $ Section sec [hdr, gr]
    where
      gc = applyAlg collapseGraph cd
      p = "codeCollapsed"
      lbl = "Collapsed view of the entire codebase"
      hdr = Paragraph [Text "The collapsed view of code collapses \
                            \down all cliques, cycles, chains, etc. to \
                            \make the graph tree-like." ]
      gr = GraphImage (toGraph p lbl gc)
      sec = Text "Collapsed view of the entire codebase"
