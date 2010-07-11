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
   Module      : Analyse.Module
   Description : Analyse modules.
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : GPL-3 or later.
   Maintainer  : Ivan.Miljenovic@gmail.com

   Analysis of Haskell modules.
 -}
module Analyse.Module(analyseModules) where

import Parsing.Types
import Analyse.Utils
import Analyse.GraphRepr
import Analyse.Visualise

import Data.Graph.Analysis

import Data.Maybe(mapMaybe)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.MultiSet as MS
import Text.Printf(printf)

-- -----------------------------------------------------------------------------

-- Helper types

-- | Shorthand type
type ModuleData = (String, ModName, HData')

-- -----------------------------------------------------------------------------

-- Analysing.

-- | Performs analysis of all modules present in the 'ParsedModules' provided.
analyseModules :: ParsedModules -> DocElement
analyseModules = Section (Text "Analysis of each module")
                 . mapMaybe analyseModule . M.elems

-- | Performs analysis of the given 'ParsedModule'.
analyseModule    :: ParsedModule -> Maybe DocElement
analyseModule hm = if n > 1
                   then Just $ Section sec elems
                   else Nothing
    where
      (n,fd@(m,_,_)) = moduleToGraph hm
      elems = mapMaybe ($fd) [ graphOf
                             -- , collapseAnal
                             , coreAnal
                             , levelAnal
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
moduleToGraph    :: ParsedModule -> (Int,ModuleData)
moduleToGraph hm = (n,(nameOfModule mn, mn, mkHData' vs fd))
    where
      mn = moduleName hm
      n = applyAlg noNodes fd
      fd = importData params
      vs = virtualEnts hm
      params = ImpParams { dataPoints    = S.toList $ internalEnts hm
                         , relationships = MS.toList . MS.map callToRel
                                           $ funcCalls hm
                         , roots         = S.toList $ exports hm
                         , directed      = True
                         }

graphOf          :: ModuleData -> Maybe DocElement
graphOf (n,m,fd) = Just $ Section sec [gi]
    where
      sec = Grouping [ Text "Visualisation of"
                     , Emphasis $ Text n]
      gi = GraphImage $ DG n (Text lbl) (drawGraph lbl (Just m) fd)
      lbl = unwords ["Diagram of:", n]

componentAnal :: ModuleData -> Maybe DocElement
componentAnal (n,_,fd)
    | single comp = Nothing
    | otherwise   = Just el
    where
      -- Use compactData as the number of edges don't matter, just
      -- whether or not an edge exists.
      comp = applyAlg componentsOf . compactData $ collapsedHData fd
      len = length comp
      el = Section sec [Paragraph [Text text]]
      sec = Grouping [ Text "Component analysis of"
                     , Emphasis (Text n)]
      text = printf "The module %s has %d components.  \
                     \You may wish to consider splitting it up." n len

cliqueAnal :: ModuleData -> Maybe DocElement
cliqueAnal (n,_,fd)
    | null clqs = Nothing
    | otherwise = Just el
    where
      clqs = applyAlg cliquesIn . onlyNormalCalls' . compactData
             $ collapsedHData fd
      clqs' = return . Itemized
              $ map (Paragraph . return . Text . showNodes' (name . snd)) clqs
      text = Text $ printf "The module %s has the following cliques:" n
      el = Section sec $ Paragraph [text] : clqs'
      sec = Grouping [ Text "Clique analysis of"
                     , Emphasis (Text n)]

cycleAnal :: ModuleData -> Maybe DocElement
cycleAnal (n,_,fd)
    | null cycs = Nothing
    | otherwise = Just el
    where
      cycs = applyAlg uniqueCycles . onlyNormalCalls' . compactData
             $ collapsedHData fd
      cycs' = return . Itemized
              $ map (Paragraph . return . Text . showCycle' (name . snd)) cycs
      text = Text $ printf "The module %s has the following non-clique \
                            \cycles:" n
      el = Section sec $ Paragraph [text] : cycs'
      sec = Grouping [ Text "Cycle analysis of"
                     , Emphasis (Text n)]

chainAnal :: ModuleData -> Maybe DocElement
chainAnal (n,_,fd)
    | null chns = Nothing
    | otherwise = Just el
    where
      chns = interiorChains . onlyNormalCalls' . compactData
             $ collapsedHData fd
      chns' = return . Itemized
              $ map (Paragraph . return . Text . showPath' (name . snd)) chns
      text = Text $ printf "The module %s has the following chains:" n
      textAfter = Text "These chains can all be compressed down to \
                       \a single function."
      el = Section sec
           $ [Paragraph [text]] ++ chns' ++ [Paragraph [textAfter]]
      sec = Grouping [ Text "Chain analysis of"
                     , Emphasis (Text n)]

rootAnal :: ModuleData -> Maybe DocElement
rootAnal (n,_,fd)
    | asExpected = Nothing
    | otherwise  = Just $ Section sec inaccessible
    where
      fd' = compactData $ origHData fd
      ntWd = S.toList . inaccessibleNodes $ addImplicit (origVirts fd) fd'
      ntWd' = applyAlg getLabels fd' ntWd
      asExpected = null ntWd
      inaccessible = [ Paragraph [Text "These functions are those that are inaccessible:"]
                    , Paragraph [Emphasis . Text $ showNodes' name ntWd']
                    ]
      sec = Grouping [ Text "Root analysis of"
                     , Emphasis (Text n)]

coreAnal          :: ModuleData -> Maybe DocElement
coreAnal (n,m,fd) = fmap mkDE $ makeCore fd
    where
      mkDE fd' = Section sec [ hdr
                             , GraphImage $ DG p
                                               (Text lbl)
                                               (drawGraph lbl (Just m) fd')
                             ]
      p = n ++ "_core"
      lbl = unwords ["Core of", n]
      hdr = coreDesc "a module"
      sec = Grouping [ Text "Core analysis of"
                     , Emphasis (Text n)]

levelAnal          :: ModuleData -> Maybe DocElement
levelAnal (n,m,fd) = Just $ Section sec [hdr, lvls]
  where
    lvls = GraphImage $ DG p (Text lbl) (drawLevels lbl (Just m) fd)
    p = n ++ "_levels"
    lbl = unwords ["Levels within", n]
    sec = Grouping [ Text "Visualisation of levels in"
                   , Emphasis (Text n)
                   ]
    hdr = Paragraph [Text "Visualises how far away from the exported root\
                           \ entities an entity is."]

-- Comment out until can work out a way of dealing with [Entity] for
-- the node-label type.
{-
collapseAnal          :: ModuleData -> Maybe DocElement
collapseAnal (n,m,fd) = if (trivialCollapse gc)
                        then Nothing
                        else Just el
    where
      fd' = updateGraph collapseGraph fd
      gc = graph fd'
      p = n ++ "_collapsed"
      lbl = unwords ["Collapsed view of", n]
      hdr = Paragraph [Text "The collapsed view of a module collapses \
                            \down all cliques, cycles, chains, etc. to \
                            \make the graph tree-like." ]
      gr = GraphImage (p,Text lbl,drawGraph lbl (Just m) fd')
      el = Section sec [hdr, gr]
      sec = Grouping [ Text "Collapsed view of"
                     , Emphasis (Text n)]
-}

cycleCompAnal          :: ModuleData -> Maybe DocElement
cycleCompAnal (n,_,fd) = Just $ Section sec pars
    where
      cc = cyclomaticComplexity . graphData $ collapsedHData fd
      sec = Grouping [ Text "Cyclomatic Complexity of"
                       , Emphasis (Text n)]
      pars = [Paragraph [text], Paragraph [textAfter, link]]
      text = Text
             $ printf "The cyclomatic complexity of %s is: %d." n cc
      textAfter = Text "For more information on cyclomatic complexity, \
                       \please see: "
      link = DocLink (Text "Wikipedia: Cyclomatic Complexity")
                     (Web "http://en.wikipedia.org/wiki/Cyclomatic_complexity")
