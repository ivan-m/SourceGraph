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

type ImportData = GraphData ModuleName

-- | Analyse the imports present in the software.  Takes in a random seed
--   as well as a list of all modules exported.
analyseImports         :: [ModuleName] -> HaskellModules -> DocElement
analyseImports exps hm = Section sec elems
    where
      imd = importsToGraph exps hm
      sec = Text "Analysis of module imports"
      elems = catMaybes
              $ map ($imd) [ graphOf
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

graphOf     :: ImportData -> Maybe DocElement
graphOf imd = Just $ Section sec [gi]
    where
      sec = Text "Visualisation of imports"
      gi = GraphImage $ applyAlg dg imd
      dg g = toGraph "imports" lbl g
      lbl = "Import visualisation"

componentAnal :: ImportData -> Maybe DocElement
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

cycleAnal :: ImportData -> Maybe DocElement
cycleAnal imd
    | null cycs = Nothing
    | otherwise = Just el
    where
      cycs = applyAlg cyclesIn imd
      cycs' = return . Enumeration
              $ map (Paragraph . return . Text . showCycle) cycs
      text = Text "The imports have the following cycles:"
      textAfter = Text "Whilst this is valid, it may make it difficult \
                       \to use in ghci, etc."
      el = Section sec
           $ (Paragraph [text]) : cycs' ++ [Paragraph [textAfter]]
      sec = Text "Cycle analysis of imports"

chainAnal :: ImportData -> Maybe DocElement
chainAnal imd
    | null chns = Nothing
    | otherwise = Just el
    where
      chns = interiorChains imd
      chns' = return . Enumeration
              $ map (Paragraph . return . Text . showPath) chns
      text = Text "The imports have the following chains:"
      textAfter = Text "These chains can all be compressed down to \
                       \a single module."
      el = Section sec $
           [Paragraph [text]] ++ chns' ++ [Paragraph [textAfter]]
      sec = Text "Import chain analysis"

rootAnal :: ImportData -> Maybe DocElement
rootAnal imd
    | asExpected = Nothing
    | otherwise  = Just $ Section sec ps
    where
      (wntd, ntRs, ntWd) = classifyRoots imd
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
      sec = Text "Import root analysis"

cycleCompAnal     :: ImportData -> Maybe DocElement
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
                     (URL "http://en.wikipedia.org/wiki/Cyclomatic_complexity")
