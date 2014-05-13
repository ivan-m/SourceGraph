{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

{-
Copyright (C) 2010 Ivan Lazar Miljenovic <Ivan.Miljenovic@gmail.com>

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
   Module      : Analyse.GraphRepr
   Description : Interacting with GraphData
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : GPL-3 or later.
   Maintainer  : Ivan.Miljenovic@gmail.com

   Interacting with GraphData from Graphalyze.
 -}
module Analyse.GraphRepr
       ( -- * General stuff
         GData(..)
       , mapData
       , mapData'
         -- * Entity-based
       , HData'
       , mkHData'
       , origHData
       , origVirts
       , collapsedHData
       , collapsedVirts
       , updateOrig
       , updateCollapsed
       , makeCore
       , HData
       , mkHData
       , HSData
       , HSClustData
       , HSGraph
       , HSClustGraph
         -- ** Utility functions
       , addImplicit
       , implicitExports
       , onlyNormalCalls
       , onlyNormalCalls'
         -- * Import-based
       , MData
       , mkMData
       , ModData
       , ModGraph
       ) where

import Analyse.Colors
import Analyse.Utils
import Parsing.Types

import Data.Graph.Analysis
import Data.Graph.Inductive
import Data.GraphViz.Attributes (X11Color)

import           Control.Monad (liftM2)
import           Data.List     (isPrefixOf)
import           Data.Map      (Map)
import qualified Data.Map      as M
import           Data.Maybe    (mapMaybe)
import           Data.Set      (Set)
import qualified Data.Set      as S

-- -----------------------------------------------------------------------------

data GData n e = GD { graphData   :: GraphData n e
                    , compactData :: GraphData n (Int, e)
                    , nodeCols    :: [(Set Node, X11Color)]
                    , edgeCols    :: [(Set Edge, X11Color)]
                    }
                 deriving (Eq, Show, Read)

mkGData       :: (Ord e) => (GraphData n e -> [(Set Node, X11Color)])
                 -> (GraphData n e -> [(Set Edge, X11Color)])
                 -> GraphData n e -> GData n e
mkGData n e g = GD { graphData   = g
                   , compactData = updateGraph compactSame g
                   , nodeCols    = n g
                   , edgeCols    = e g
                   }

-- | Does not touch the 'nodeCols' values.  Should only touch the labels.
mapData      :: (Ord e') => (GraphData n e -> GraphData n' e')
                -> GData n e -> GData n' e'
mapData f gd = GD { graphData   = gr'
                  , compactData = updateGraph compactSame gr'
                  , nodeCols    = nodeCols gd
                  , edgeCols    = edgeCols gd
                  }
  where
    gr = graphData gd
    gr' = f gr

mapData'   :: (Ord e') => (AGr n e -> AGr n' e') -> GData n e -> GData n' e'
mapData' f = mapData (updateGraph f)

commonColors    :: GraphData n e -> [(Set Node, X11Color)]
commonColors gd = [ (rs', exportedRootColor)
                  , (es, exportedInnerColor)
                  , (ls, leafColor)
                  ]
  where
    rs' = S.intersection rs es
    rs = getRoots  gd
    ls = getLeaves gd
    es = getWRoots gd

getRoots :: GraphData a b -> Set Node
getRoots = S.fromList . applyAlg rootsOf'

getLeaves :: GraphData a b -> Set Node
getLeaves = S.fromList . applyAlg leavesOf'

getWRoots :: GraphData a b -> Set Node
getWRoots = S.fromList . wantedRootNodes

-- -----------------------------------------------------------------------------

data HData' = HD' { origHData      :: HData
                  , origVirts      :: Set Entity
                  , collapsedHData :: HData
                  , collapsedVirts :: Set Entity
                  }
              deriving (Eq, Show, Read)

mkHData'       :: Set Entity -> HSData -> HData'
mkHData' vs hs = HD' { origHData      = mkHData vs hs
                     , origVirts      = vs
                     , collapsedHData = mkHData vs' hs'
                     , collapsedVirts = vs'
                     }
  where
    (hs', repLookup) = collapseStructures hs
    vs' = S.fromList
          . mapMaybe (flip M.lookup repLookup)
          $ S.toList vs

-- | Doesn't touch origVirts
updateOrig       :: (HSGraph -> HSGraph) -> HData' -> HData'
updateOrig f hd' = hd' { origHData = mapData' f $ origHData hd' }

-- | Doesn't touch collapsedVirts
updateCollapsed       :: (HSGraph -> HSGraph) -> HData' -> HData'
updateCollapsed f hd' = hd' { collapsedHData = mapData' f $ collapsedHData hd' }

makeCore    :: HData' -> Maybe HData'
makeCore hd = bool Nothing (Just hd') $ isInteresting hd'
  where
    isInteresting = not . applyAlg (liftM2 (||) isEmpty unChanged)
                    . graphData . origHData
    unChanged = applyAlg equal $ graphData (origHData hd)
    hd' = updateOrig coreOf hd

type HData = GData Entity CallType

mkHData    :: Set Entity -> HSData -> HData
mkHData vs = mkGData (entColors vs) (callColors . onlyNormalCalls)

type HSData = GraphData Entity CallType
type HSClustData = GraphData (GenCluster Entity) CallType
type HSGraph = AGr Entity CallType
type HSClustGraph = AGr (GenCluster Entity) CallType

entColors       :: Set Entity -> GraphData Entity e -> [(Set Node, X11Color)]
entColors vs hd = (us, inaccessibleColor)
                  :
                  commonColors hd
                  ++
                  -- Do this after in case there's an implicit export
                  -- that is explicitly exported.
                  [ (imps, implicitExportColor)
                  ]
  where
    hd' = addImplicit vs hd
    us = inaccessibleNodes hd'
    imps = implicitExports vs hd

callColors    :: HSData -> [(Set Edge, X11Color)]
callColors hd = [ (cliqueEdges clqs, cliqueColor)
                , (cycleEdges  cycs, cycleColor)
                , (chainEdges  chns, chainColor)
                ]
  where
    clqs = applyAlg cliquesIn' hd
    cycs = applyAlg uniqueCycles' hd
    chns = applyAlg chainsIn' hd

-- -----------------------------------------------------------------------------

onlyNormalCalls :: HSData -> HSData
onlyNormalCalls = updateGraph go
    where
      go = elfilter isNormalCall

onlyNormalCalls' :: GraphData Entity (Int, CallType)
                    -> GraphData Entity (Int, CallType)
onlyNormalCalls' = updateGraph go
  where
    go = elfilter (isNormalCall . snd)

isImplicitExport    :: Set Entity -> LNode Entity -> Bool
isImplicitExport vs = liftM2 (||) underscoredEntity (virtClass vs) . label

-- | Various warnings about unused/unexported entities are suppressed
--   if they start with an underscore:
--   http://www.haskell.org/ghc/docs/latest/html/users_guide/options-sanity.html
underscoredEntity :: Entity -> Bool
underscoredEntity = isPrefixOf "_" . name

virtClass      :: Set Entity -> Entity -> Bool
virtClass vs e = case eType e of
                   ClassMethod{}    -> isVirt
                   CollapsedClass{} -> isVirt
                   _                -> False
  where
    isVirt = e `S.member` vs

addImplicit    :: Set Entity -> GraphData Entity e -> GraphData Entity e
addImplicit vs = addRootsBy (isImplicitExport vs)

implicitExports    :: Set Entity -> GraphData Entity e -> Set Node
implicitExports vs = S.fromList
                     . map node
                     . applyAlg (filterNodes (const (isImplicitExport vs)))

-- | Collapse items that must be kept together before clustering, etc.
--   Also updates wantedRootNodes.
collapseStructures :: HSData -> (HSData, Map Entity Entity)
collapseStructures = collapseAndUpdate' collapseFuncs

collapseFuncs :: [HSGraph -> [(NGroup, Entity)]]
collapseFuncs = [ collapseDatas
                , collapseClasses
                , collapseInsts
                ]
    where
      collapseDatas = mkCollapseTp isData getDataType mkData
      mkData m d = Ent m ("Data: " ++ d) (CollapsedData d)
      collapseClasses = mkCollapseTp isClass getClassName mkClass
      mkClass m c = Ent m ("Class: " ++ c) (CollapsedClass c)
      collapseInsts = mkCollapseTp isInstance getInstance mkInst
      mkInst m (c,d) = Ent m ("Class: " ++ c ++ ", Data: " ++ d)
                             (CollapsedInstance c d)

mkCollapseTp           :: (Ord a) => (EntityType -> Bool) -> (EntityType -> a)
                          -> (ModName -> a -> Entity) -> HSGraph
                          -> [(NGroup, Entity)]
mkCollapseTp p v mkE g = map lng2ne lngs
    where
      lns = filter (p . eType . snd) $ labNodes g
      lnas = map addA lns
      lngs = groupSortBy snd lnas
      lng2ne lng = ( map (fst . fst) lng
                   , mkEnt' $ head lng
                   )
      mkEnt' ((_,e),a) = mkE (inModule e) a
      addA ln@(_,l) = (ln, v $ eType l)

-- -----------------------------------------------------------------------------

type MData = GData ModName ()

mkMData :: ModData -> MData
mkMData = mkGData modColors modEdgeColors

type ModData = GraphData ModName ()
type ModGraph = AGr ModName ()

modColors    :: GraphData ModName e -> [(Set Node, X11Color)]
modColors gd = (us, inaccessibleColor) : commonColors gd
  where
    us = inaccessibleNodes gd

modEdgeColors    :: (Eq e) => GraphData ModName e -> [(Set Edge, X11Color)]
modEdgeColors gd = [ (cycleEdges cycs, cycleColor)
                   , (chainEdges chns, chainColor)
                   ]
  where
    cycs = applyAlg cyclesIn' gd
    chns = applyAlg chainsIn' gd


-- -----------------------------------------------------------------------------

