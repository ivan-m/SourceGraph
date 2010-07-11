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
   Module      : Analyse.Software
   Description : Analyse Haskell software
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : GPL-3 or later.
   Maintainer  : Ivan.Miljenovic@gmail.com

   Analysis of the entire overall piece of software.
 -}
module Analyse.Everything(analyseEverything) where

import Parsing.Types
import Analyse.Utils
import Analyse.GraphRepr
import Analyse.Visualise

import Data.Graph.Analysis

import Data.List(nub)
import Data.Maybe(mapMaybe)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.MultiSet as MS
import Text.Printf(printf)
import System.Random(RandomGen)

-- -----------------------------------------------------------------------------

-- | Performs analysis of the entire codebase.
analyseEverything           :: (RandomGen g) => g -> [ModName] -> ParsedModules
                            -> DocElement
analyseEverything g exps hm = Section sec elems
    where
      cd = codeToGraph exps hm
      sec = Text "Analysis of the entire codebase"
      elems = mapMaybe ($cd) [ graphOf
                             , clustersOf g
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


codeToGraph          :: [ModName] -> ParsedModules -> HData'
codeToGraph exps pms = mkHData' vs $ importData params
    where
      pms' = M.elems pms
      exps' = S.toList . S.unions
              . map exports $ mapMaybe (flip M.lookup pms) exps
      ents = S.toList . S.unions
             $ map internalEnts pms'
      calls = MS.toList . MS.map callToRel . MS.unions
              $ map funcCalls pms'
      vs = S.filter (not . internalEntity)
           . S.unions $ map virtualEnts pms'
      params = ImpParams { dataPoints    = ents
                         , relationships = calls
                         , roots         = exps'
                         , directed      = True
                         }

graphOf    :: HData' -> Maybe DocElement
graphOf cd = Just $ Section sec [gc]
    where
      sec = Text "Visualisation of the entire software"
      gc = GraphImage $ DG "code" (Text lbl) (drawGraph lbl Nothing cd)
      lbl = "Entire Codebase"

clustersOf      :: (RandomGen g) => g -> HData' -> Maybe DocElement
clustersOf g cd = Just $ Section sec [ text, gc, textAfter
                                     , blank, cwMsg, cw] -- , blank, rngMsg, rng]
    where
      blank = Paragraph [BlankSpace]
      sec = Text "Visualisation of overall function calls"
      gc = GraphImage $ DG "codeCluster" (Text lbl) (drawGraph' lbl cd)
      text = Paragraph
             [Text "Here is the current module grouping of functions:"]
      lbl = "Current module groupings"
      textAfter = Paragraph [Text "Here is a proposed alternate module grouping:"]
                  -- [Text "Here are two proposed module groupings:"]
      cwMsg = Paragraph [Emphasis $ Text "Using the Chinese Whispers algorithm:"]
      cw = GraphImage $ DG "codeCW" (Text cwLbl) (drawClusters cwLbl (chineseWhispers g) cd)
      cwLbl = "Chinese Whispers module suggestions"
{-
      rngMsg = Paragraph [Emphasis $ Text "Using the Relative Neighbourhood algorithm:"]
      rng = GraphImage $ DG "codeRNG" (Text rngLbl) (drawClusters lbl relNbrhd cd)
      -- Being naughty to avoid having to define drawClusters'
      relNbrhd = relativeNeighbourhood $ directedData cd
      rngLbl = "Relative Neighbourhood module suggestions"
-}

componentAnal :: HData' -> Maybe DocElement
componentAnal cd
    | single comp = Nothing
    | otherwise   = Just $ Section sec [Paragraph [Text text]]
    where
      -- Use compactData as the number of edges don't matter, just
      -- whether or not an edge exists.
      comp = applyAlg componentsOf . compactData $ collapsedHData cd
      len = length comp
      sec = Text "Function component analysis"
      text = printf "The functions are split up into %d components.  \
                     \You may wish to consider splitting the code up \
                     \into multiple libraries." len

cliqueAnal :: HData' -> Maybe DocElement
cliqueAnal cd
    | null clqs = Nothing
    | otherwise = Just el
    where
      clqs = onlyCrossModule . applyAlg cliquesIn
             . onlyNormalCalls' . compactData $ collapsedHData cd
      clqs' = return . Itemized
              $ map (Paragraph . return . Text . showNodes' (fullName . snd)) clqs
      text = Text "The code has the following cross-module cliques:"
      el = Section sec $ Paragraph [text] : clqs'
      sec = Text "Overall clique analysis"

cycleAnal :: HData' -> Maybe DocElement
cycleAnal cd
    | null cycs = Nothing
    | otherwise = Just el
    where
      cycs = onlyCrossModule . applyAlg uniqueCycles
             . onlyNormalCalls' . compactData $ collapsedHData cd
      cycs' = return . Itemized
              $ map (Paragraph . return . Text . showCycle' (fullName . snd)) cycs
      text = Text "The code has the following cross-module non-clique cycles:"
      el = Section sec $ Paragraph [text] : cycs'
      sec = Text "Overall cycle analysis"

chainAnal :: HData' -> Maybe DocElement
chainAnal cd
    | null chns = Nothing
    | otherwise = Just el
    where
      chns = onlyCrossModule . interiorChains
             . onlyNormalCalls' . compactData $ collapsedHData cd
      chns' = return . Itemized
              $ map (Paragraph . return . Text . showPath' (fullName . snd)) chns
      text = Text "The code has the following cross-module chains:"
      textAfter = Text "These chains can all be compressed down to \
                       \a single function."
      el = Section sec $
           [Paragraph [text]] ++ chns' ++ [Paragraph [textAfter]]
      sec = Text "Overall chain analysis"

rootAnal :: HData' -> Maybe DocElement
rootAnal cd
    | asExpected = Nothing
    | otherwise  = Just $ Section sec inaccessible
    where
      cd' = compactData $ origHData cd
      ntWd = S.toList . inaccessibleNodes $ addImplicit (origVirts cd) cd'
      ntWd' = applyAlg getLabels cd' ntWd
      asExpected = null ntWd
      inaccessible = [ Paragraph [Text "These functions are those that are inaccessible:"]
                    , Paragraph [Emphasis . Text $ showNodes' fullName ntWd']
                    ]
      sec = Text "Overall root analysis"


cycleCompAnal    :: HData' -> Maybe DocElement
cycleCompAnal cd = Just $ Section sec pars
    where
      cc = cyclomaticComplexity . graphData $ collapsedHData cd
      sec = Text "Overall Cyclomatic Complexity"
      pars = [Paragraph [text], Paragraph [textAfter, link]]
      text = Text
             $ printf "The overall cyclomatic complexity is: %d" cc
      textAfter = Text "For more information on cyclomatic complexity, \
                       \please see: "
      link = DocLink (Text "Wikipedia: Cyclomatic Complexity")
                     (Web "http://en.wikipedia.org/wiki/Cyclomatic_complexity")


coreAnal :: HData' -> Maybe DocElement
coreAnal = fmap mkDE . makeCore
    where
      mkDE cd' = Section sec [ hdr
                             , GraphImage $ DG "codeCore"
                                               (Text lbl)
                                               (drawGraph lbl Nothing cd')
                             ]
      lbl = "Overall core"
      hdr = coreDesc "software"
      sec = Text "Overall Core analysis"

levelAnal    :: HData' -> Maybe DocElement
levelAnal cd = Just $ Section sec [hdr, lvls]
  where
    lvls = GraphImage $ DG p (Text lbl) (drawLevels lbl Nothing cd)
    p = "code_levels"
    lbl = unwords ["Levels within software"]
    sec = Grouping [Text "Visualisation of levels in the software"]
    hdr = Paragraph [Text "Visualises how far away from the exported root\
                           \ entities an entity is."]


-- Comment out until can work out a way of dealing with [Entity] for
-- the node-label type.
{-
collapseAnal    :: HData' -> Maybe DocElement
collapseAnal cd = if (trivialCollapse gc)
                  then Nothing
                  else Just $ Section sec [hdr, gr]
    where
      cd' = updateGraph collapseGraph cd
      gc = graph cd'
      lbl = "Collapsed view of the entire codebase"
      hdr = Paragraph [Text "The collapsed view of code collapses \
                            \down all cliques, cycles, chains, etc. to \
                            \make the graph tree-like." ]
      gr = GraphImage $ DG "codeCollapsed" (Text lbl) (drawGraph lbl Nothing cd')
      sec = Text "Collapsed view of the entire codebase"
-}

-- | Only use those values that are in more than one module.
onlyCrossModule :: [LNGroup Entity] -> [LNGroup Entity]
onlyCrossModule = filter crossModule
    where
      crossModule = not . single . nub . map (inModule . label)
