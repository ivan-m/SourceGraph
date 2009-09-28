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
   Module      : Analyse.Imports
   Description : Analyse module imports.
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : GPL-3 or later.
   Maintainer  : Ivan.Miljenovic@gmail.com

   Analysis of Haskell module importing.
 -}
module Analyse.Imports (analyseImports) where

import Parsing.Types
import Analyse.Utils

import Data.Graph.Analysis

import Data.Maybe(mapMaybe)
import qualified Data.Map as M
import Text.Printf(printf)

-- -----------------------------------------------------------------------------

-- | Analyse the imports present in the software.  Takes in a random seed
--   as well as a list of all modules exported.
analyseImports         :: [ModName] -> ParsedModules -> DocElement
analyseImports exps hm = Section sec elems
    where
      imd = importsToGraph exps hm
      sec = Text "Analysis of module imports"
      elems = mapMaybe ($imd) [ graphOf
                              , cycleCompAnal
                              , rootAnal
                              , componentAnal
                              , cycleAnal
                              , chainAnal
                              ]

importsToGraph          :: [ModName] -> ParsedModules -> ModData
importsToGraph exps pms = importData params
    where
      params = Params { dataPoints    = M.keys pms
                      , relationships = moduleImports pms
                      , roots         = exps
                      , directed      = True
                      }

moduleImports :: ParsedModules -> [Rel ModName ()]
moduleImports = concatMap imps . M.elems
    where
      imps pm = map (toRel (moduleName pm))
                . M.keys $ imports pm
      toRel m m' = (m,m',())

graphOf     :: ModData -> Maybe DocElement
graphOf imd = Just $ Section sec [gi]
    where
      sec = Text "Visualisation of imports"
      gi = GraphImage ("imports", lbl, drawModules imd)
      lbl = Text "Import visualisation"

componentAnal :: ModData -> Maybe DocElement
componentAnal imd
    | single comp = Nothing
    | otherwise   = Just el
    where
      comp = applyAlg componentsOf imd
      len = length comp
      el = Section sec [Paragraph [Text text]]
      sec = Text "Import component analysis"
      text = printf "The imports have %d components.  \
                     \You may wish to consider splitting the code up." len

cycleAnal :: ModData -> Maybe DocElement
cycleAnal imd
    | null cycs = Nothing
    | otherwise = Just el
    where
      cycs = applyAlg cyclesIn imd
      cycs' = return . Itemized
              $ map (Paragraph . return . Text . showCycle) cycs
      text = Text "The imports have the following cycles:"
      textAfter = Text "Whilst this is valid, it may make it difficult \
                       \to use in ghci, etc."
      el = Section sec
           $ Paragraph [text] : cycs' ++ [Paragraph [textAfter]]
      sec = Text "Cycle analysis of imports"

chainAnal :: ModData -> Maybe DocElement
chainAnal imd
    | null chns = Nothing
    | otherwise = Just el
    where
      chns = interiorChains imd
      chns' = return . Itemized
              $ map (Paragraph . return . Text . showPath) chns
      text = Text "The imports have the following chains:"
      textAfter = Text "These chains can all be compressed down to \
                       \a single module."
      el = Section sec $
           [Paragraph [text]] ++ chns' ++ [Paragraph [textAfter]]
      sec = Text "Import chain analysis"

rootAnal :: ModData -> Maybe DocElement
rootAnal imd
    | asExpected = Nothing
    | otherwise  = Just $ Section sec ps
    where
      (wntd, ntRs, ntWd) = classifyRoots imd
      asExpected = null ntRs && null ntWd
      rpt (s,ns) = if null ns
                   then Nothing
                   else Just [ Paragraph
                               [Text
                                $ concat ["These modules are those that are "
                                         , s, ":"]]
                             , Paragraph [Emphasis . Text $ showNodes ns]]
      ps = concat
           $ mapMaybe rpt [ ("in the export list and roots",wntd)
                          , ("in the export list but not roots",ntRs)
                          , ("not in the export list but roots",ntWd)]
      sec = Text "Import root analysis"

cycleCompAnal     :: ModData -> Maybe DocElement
cycleCompAnal imd = Just $ Section sec pars
    where
      cc = cyclomaticComplexity imd
      sec = Text "Cyclomatic Complexity of imports"
      pars = [Paragraph [text], Paragraph [textAfter, link]]
      text = Text
             $ printf "The cyclomatic complexity of the imports is: %d" cc
      textAfter = Text "For more information on cyclomatic complexity, \
                       \please see: "
      link = DocLink (Text "Wikipedia: Cyclomatic Complexity")
                     (Web "http://en.wikipedia.org/wiki/Cyclomatic_complexity")
