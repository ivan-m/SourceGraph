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
   Module      : Analyse.Module
   Description : Analyse modules.
   Copyright   : (c) Ivan Lazar Miljenovic 2008
   License     : GPL-3 or later.
   Maintainer  : Ivan.Miljenovic@gmail.com

   Analysis of Haskell modules.
 -}
module Analyse.Module(analyseModules) where

import Parsing.Types
import Analyse.Utils

import Data.Graph.Analysis

import Data.Maybe
import Text.Printf

-- -----------------------------------------------------------------------------

-- Helper types

-- | Shorthand type
type FunctionData = (String, GraphData AString)

-- -----------------------------------------------------------------------------

-- Analysing.

-- | Performs analysis of all modules present in the 'HaskellModules' provided.
analyseModules :: HaskellModules -> DocElement
analyseModules = Section (Text "Analysis of each module")
                 . catMaybes . map analyseModule . hModulesIn

-- | Performs analysis of the given 'HaskellModule'.
analyseModule    :: HaskellModule -> Maybe DocElement
analyseModule hm = if (n > 1)
                   then Just $  Section sec elems
                   else Nothing
    where
      m = show $ moduleName hm
      (n,fd) = moduleToGraph hm
      elems = catMaybes
              $ map ($fd) [ graphOf
                          , collapseAnal
                          , coreAnal
                          , cycleCompAnal
                          , rootAnal
                          , componentAnal
                          , cliqueAnal
                          , cycleAnal
                          , chainAnal
                          ]
      sec = Grouping [ Text "Analysis of"
                     , Emphasis (Text m)]

-- | Convert the module to the /Graphalyze/ format.
moduleToGraph    :: HaskellModule -> (Int,FunctionData)
moduleToGraph hm = (n,(show $ moduleName hm, fd'))
    where
      n = applyAlg noNodes fd'
      fd' = manipulateNodes (AS . name) fd
      fd = importData params
      funcs = functions hm
      params = Params { dataPoints    = functionsIn funcs
                      , relationships = functionEdges funcs
                      , roots         = exports hm
                      , directed      = True
                      }

graphOf        :: FunctionData -> Maybe DocElement
graphOf (m,fd) = Just $ Section sec [gi]
    where
      sec = Grouping [ Text "Visualisation of"
                     , Emphasis (Text m)]
      gi = GraphImage $ applyAlg dg fd
      dg g = toGraph m lbl g
      lbl = unwords ["Diagram of:", m]

componentAnal :: FunctionData -> Maybe DocElement
componentAnal (m,fd)
    | single comp = Nothing
    | otherwise   = Just el
    where
      comp = applyAlg componentsOf fd
      len = length comp
      el = Section sec [Paragraph [Text text]]
      sec = Grouping [ Text "Component analysis of"
                     , Emphasis (Text m)]
      text = printf "The module %s has %d components.  \
                     \You may wish to consider splitting it up." m len

cliqueAnal :: FunctionData -> Maybe DocElement
cliqueAnal (m,fd)
    | null clqs = Nothing
    | otherwise = Just el
    where
      clqs = applyAlg cliquesIn fd
      clqs' = return . Itemized
              $ map (Paragraph . return . Text . showNodes) clqs
      text = Text $ printf "The module %s has the following cliques:" m
      el = Section sec $ (Paragraph [text]) : clqs'
      sec = Grouping [ Text "Clique analysis of"
                     , Emphasis (Text m)]

cycleAnal :: FunctionData -> Maybe DocElement
cycleAnal (m,fd)
    | null cycs = Nothing
    | otherwise = Just el
    where
      cycs = applyAlg uniqueCycles fd
      cycs' = return . Itemized
              $ map (Paragraph . return . Text . showCycle) cycs
      text = Text $ printf "The module %s has the following non-clique \
                            \cycles:" m
      el = Section sec $ (Paragraph [text]) : cycs'
      sec = Grouping [ Text "Cycle analysis of"
                     , Emphasis (Text m)]

chainAnal :: FunctionData -> Maybe DocElement
chainAnal (m,fd)
    | null chns = Nothing
    | otherwise = Just el
    where
      chns = interiorChains fd
      chns' = return . Itemized
              $ map (Paragraph . return . Text . showPath) chns
      text = Text $ printf "The module %s has the following chains:" m
      textAfter = Text "These chains can all be compressed down to \
                       \a single function."
      el = Section sec
           $ [Paragraph [text]] ++ chns' ++ [Paragraph [textAfter]]
      sec = Grouping [ Text "Chain analysis of"
                     , Emphasis (Text m)]

rootAnal :: FunctionData -> Maybe DocElement
rootAnal (m,fd)
    | asExpected = Nothing
    | otherwise  = Just el
    where
      (wntd, ntRs, ntWd) = classifyRoots fd
      asExpected = (null ntRs) && (null ntWd)
      rpt (s,ns) = if (null ns)
                   then Nothing
                   else Just [ Paragraph
                               [Text
                                $ concat ["These nodes are those that are "
                                         , s, ":"]]
                             , Paragraph [Emphasis . Text $ showNodes ns]]
      ps = concat . catMaybes
           $ map rpt [ ("in the export list and roots",wntd)
                     , ("in the export list but not roots",ntRs)
                     , ("not in the export list but roots",ntWd)]
      el = Section sec ps
      sec = Grouping [ Text "Root analysis of"
                     , Emphasis (Text m)]

coreAnal        :: FunctionData -> Maybe DocElement
coreAnal (m,fd) = if (isEmpty core)
                  then Nothing
                  else Just el
    where
      core = applyAlg coreOf fd
      p = m ++ "_core"
      lbl = unwords ["Core of", m]
      hdr = Paragraph [Text "The core of a module can be thought of as \
                             \the part where all the work is actually done."]
      anal = GraphImage (toGraph p lbl core)
      el = Section sec [hdr, anal]
      sec = Grouping [ Text "Core analysis of"
                     , Emphasis (Text m)]

collapseAnal        :: FunctionData -> Maybe DocElement
collapseAnal (m,fd) = if (trivialCollapse gc)
                      then Nothing
                      else Just el
    where
      gc = applyAlg collapseGraph fd
      p = m ++ "_collapsed"
      lbl = unwords ["Collapsed view of", m]
      hdr = Paragraph [Text "The collapsed view of a module collapses \
                            \down all cliques, cycles, chains, etc. to \
                            \make the graph tree-like." ]
      gr = GraphImage (toGraph p lbl gc)
      el = Section sec [hdr, gr]
      sec = Grouping [ Text "Collapsed view of"
                     , Emphasis (Text m)]


cycleCompAnal        :: FunctionData -> Maybe DocElement
cycleCompAnal (m,fd) = Just $ Section sec pars
    where
      cc = cyclomaticComplexity fd
      sec = Grouping [ Text "Cyclomatic Complexity of"
                       , Emphasis (Text m)]
      pars = [Paragraph [text], Paragraph [textAfter, link]]
      text = Text
             $ printf "The cyclomatic complexity of %s is: %d." m cc
      textAfter = Text "For more information on cyclomatic complexity, \
                       \please see: "
      link = DocLink (Text "Wikipedia: Cyclomatic Complexity")
                     (URL "http://en.wikipedia.org/wiki/Cyclomatic_complexity")
