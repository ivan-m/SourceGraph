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
analyseModules :: HaskellModules -> [DocElement]
analyseModules = map analyseModule . hModulesIn

-- | Performs analysis of the given 'HaskellModule'.
analyseModule    :: HaskellModule -> DocElement
analyseModule hm = Section title elems
    where
      m = show $ moduleName hm
      fd = moduleToGraph hm
      elems = catMaybes
              $ map ($fd) [ graphOf
                          , collapseAnal
                          , coreAnal
                          , rootAnal
                          , componentAnal
                          , cliqueAnal
                          , cycleAnal
                          , chainAnal
                          ]
      title = Grouping [ Text "Analysis of"
                       , Emphasis (Text m)]

-- | Convert the module to the /Graphalyze/ format.
moduleToGraph    :: HaskellModule -> FunctionData
moduleToGraph hm = (show $ moduleName hm, fd')
    where
      fd' = manipulateNodes (AS . name) fd
      fd = importData params
      funcs = functions hm
      params = Params { dataPoints    = functionsIn funcs
                      , relationships = functionEdges funcs
                      , roots         = exports hm
                      , directed      = True
                      }

graphOf        :: FunctionData -> Maybe DocElement
graphOf (m,fd) = Just $ Section title [gi]
    where
      title = Grouping [ Text "Visualisation of"
                       , Emphasis (Text m)]
      gi = GraphImage $ applyAlg dg fd
      dg g = toGraph m label g
      label = unwords ["Diagram of:", m]

componentAnal :: FunctionData -> Maybe DocElement
componentAnal (m,fd)
    | single comp = Nothing
    | otherwise   = Just elem
    where
      comp = applyAlg componentsOf fd
      len = length comp
      elem = Section title [Paragraph [Text text]]
      title = Grouping [ Text "Component analysis of"
                       , Emphasis (Text m)]
      text = printf "The module %s has %d components.  \
                     \You may wish to consider splitting it up." m len

cliqueAnal :: FunctionData -> Maybe DocElement
cliqueAnal (m,fd)
    | null clqs = Nothing
    | otherwise = Just elem
    where
      clqs = applyAlg cliquesIn fd
      clqs' = Paragraph $ map (Text . showNodes) clqs
      text = Text $ printf "The module %s has the following cliques:" m
      elem = Section title [Paragraph [text], clqs']
      title = Grouping [ Text "Clique analysis of"
                       , Emphasis (Text m)]

cycleAnal :: FunctionData -> Maybe DocElement
cycleAnal (m,fd)
    | null cycs = Nothing
    | otherwise = Just elem
    where
      cycs = applyAlg uniqueCycles fd
      cycs' = Paragraph $ map (Text . showCycle) cycs
      text = Text $ printf "The module %s has the following non-clique \
                            \cycles:" m
      elem = Section title [Paragraph [text], cycs']
      title = Grouping [ Text "Cycle analysis of"
                       , Emphasis (Text m)]

chainAnal :: FunctionData -> Maybe DocElement
chainAnal (m,fd)
    | null chns = Nothing
    | otherwise = Just elem
    where
      chns = applyAlg chainsIn fd
      chns' = Paragraph $ map (Text . showPath) chns
      text = Text $ printf "The module %s has the following chains:" m
      textAfter = Text "These chains can all be compressed down to \
                       \a single function."
      elem = Section title [Paragraph [text], chns', Paragraph [textAfter]]
      title = Grouping [ Text "Chain analysis of"
                       , Emphasis (Text m)]

rootAnal :: FunctionData -> Maybe DocElement
rootAnal (m,fd)
    | asExpected = Nothing
    | otherwise  = Just elem
    where
      (wntd, ntRs, ntWd) = classifyRoots fd
      asExpected = (null ntRs) && (null ntWd)
      rpt (s,ns) = if (null ns)
                   then Nothing
                   else Just [ Paragraph
                               [Text
                                $ concat ["These nodes are those that are "
                                         , s, ":"]]
                             , Paragraph [Text $ showNodes ns]]
      ps = concat . catMaybes
           $ map rpt [ ("in the export list and roots",wntd)
                     , ("in the export list but not roots",ntWd)
                     , ("not in the export list but roots",ntRs)]
      elem = Section title ps
      title = Grouping [ Text "Root analysis of"
                       , Emphasis (Text m)]

coreAnal        :: FunctionData -> Maybe DocElement
coreAnal (m,fd) = Just elem
    where
      core = applyAlg coreOf fd
      p = m ++ "_core"
      label = unwords ["Core of", m]
      hdr = Paragraph [Text "The core of a module can be thought of as \
                             \where all the work is actually done."]
      empMsg = Paragraph [Text $ printf "The module %s is a tree." m]
      anal = if (isEmpty core)
             then empMsg
             else GraphImage (toGraph p label core)
      elem = Section title [hdr, anal]
      title = Grouping [ Text "Core analysis of"
                       , Emphasis (Text m)]

collapseAnal :: FunctionData -> Maybe DocElement
collapseAnal (m,fd) = Just elem
    where
      gc = applyAlg collapseGraph fd
      p = m ++ "_collapsed"
      label = unwords ["Collapsed view of", m]
      hdr = Paragraph [Text "The collapsed view of a module collapses \
                            \down all cliques, cycles, chains, etc. to \
                            \make the graph tree-like." ]
      gr = GraphImage (toGraph p label gc)
      elem = Section title [hdr, gr]
      title = Grouping [ Text "Collapsed view of"
                       , Emphasis (Text m)]

