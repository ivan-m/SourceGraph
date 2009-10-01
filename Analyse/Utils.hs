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
   Module      : Analyse.Utils
   Description : Utility functions and types for analysis.
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : GPL-3 or later.
   Maintainer  : Ivan.Miljenovic@gmail.com

   Utility functions and types for analysis.
 -}
module Analyse.Utils where

import Parsing.Types

import Data.Graph.Analysis hiding (Bold)
import Data.Graph.Inductive hiding (graphviz)

import Data.GraphViz

import Data.List(groupBy, sortBy)
-- import Data.Maybe(isJust)
import Data.Function(on)
import qualified Data.Set as S
import Data.Set(Set)
import qualified Data.IntSet as I
import Data.IntSet(IntSet)

-- -----------------------------------------------------------------------------

type HSData = GraphData Entity CallType
type HSClustData = GraphData (GenCluster Entity) CallType
type HSGraph = AGr Entity CallType
type HSClustGraph = AGr (GenCluster Entity) CallType

type ModData = GraphData ModName ()
type ModGraph = AGr ModName ()

-- -----------------------------------------------------------------------------

-- | Cyclomatic complexity
cyclomaticComplexity    :: GraphData a b -> Int
cyclomaticComplexity gd = e - n + 2*p
    where
      p = length $ applyAlg componentsOf gd
      n = applyAlg noNodes gd
      e = length $ applyAlg labEdges gd

-- | Collapse items that must be kept together before clustering, etc.
collapseStructures :: HSData -> HSData
collapseStructures = updateGraph collapseStructures'

collapseStructures' :: HSGraph -> HSGraph
collapseStructures' = collapseGraphBy' [ collapseDatas
                                       , collapseClasses
                                       , collapseInsts
                                       ]
    where
      collapseDatas = mkCollapseTp isData getDataType mkData
      mkData m d = Ent m ("Data: " ++ d) (CollapsedData d)
      collapseClasses = mkCollapseTp isClass getClassName mkClass
      mkClass m c = Ent m ("Class: " ++ c) (CollapsedClass c)
      collapseInsts = mkCollapseTp isInstance getInstance mkInst
      mkInst m (c,d) = Ent m ("Class: " ++ c ++ "\\nData: " ++ d)
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
                   , mkEnt $ head lng
                   )
      mkEnt ((_,e),a) = mkE (inModule e) a
      addA ln@(_,l) = (ln, v $ eType l)

groupSortBy   :: (Ord b) => (a -> b) -> [a] -> [[a]]
groupSortBy f = groupBy ((==) `on` f) . sortBy (compare `on` f)

toSet :: (Ord a) => LNGroup a -> Set a
toSet = S.fromList . map snd

getRoots :: (Ord a) => GraphData a b -> Set a
getRoots = toSet . applyAlg rootsOf

getLeaves :: (Ord a) => GraphData a b -> Set a
getLeaves = toSet . applyAlg leavesOf

getWRoots :: (Ord a) => GraphData a b -> Set a
getWRoots = toSet . wantedRoots

bool       :: a -> a -> Bool -> a
bool t f b = if b then t else f

-- -----------------------------------------------------------------------------

-- | Create the nested 'DotGraph'.
drawGraph           :: String -> Maybe ModName -> HSData -> DotGraph Node
drawGraph gid mm dg = setID (Str gid)
                      $ graphvizClusters' dg'
                                          gAttrs
                                          toClust
                                          ctypeID
                                          clustAttributes'
                                          nAttr
                                          callAttributes'
    where
      gAttrs = [NodeAttrs [Margin . PVal $ PointD 0.2 0.2]] -- [GraphAttrs [Label $ StrLabel t]]
      dg' = updateGraph compactSame dg
      -- Possible clustering problem
      toClust = clusterEntity -- bool clusterEntity clusterEntityM' $ isJust mm
      rs = getRoots dg
      ls = getLeaves dg
      es = getWRoots dg
      nAttr = entityAttributes rs ls es False mm

-- | One-module-per-cluster 'DotGraph'
drawGraph'        :: String -> HSData -> DotGraph Node
drawGraph' gid dg = setID (Str gid)
                    $ graphvizClusters dg'
                                       gAttrs
                                       modClustAttrs
                                       nAttr
                                       callAttributes'
    where
      gAttrs = [] -- [GraphAttrs [Label $ StrLabel t]]
      dg' = updateGraph (compactSame . collapseStructures') dg
      rs = getRoots dg
      ls = getLeaves dg
      es = getWRoots dg
      nAttr = entityAttributes rs ls es False Nothing

-- | GetRoots, GetLeaves, Exported, @'Just' m@ if only one module, @'Nothing'@ if all.
--   'True' if add explicit module name to all entities.
entityAttributes :: Set Entity -> Set Entity -> Set Entity -> Bool
                    -> Maybe ModName -> LNode Entity -> Attributes
entityAttributes rs ls ex a mm (_,e@(Ent m n t))
    = [ Label $ StrLabel lbl
      , Shape $ shapeFor t
      -- , Color [ColorName cl]
      , FillColor $ ColorName sh
      , Style [SItem Filled [], styleFor mm m]
      ]
    where
      lbl = bool (nameOfModule m ++ "\\n" ++ n) n
            $ not sameMod || a
      -- Using the default X11 color names.
      {-
      isR = e `S.member` rs
      isL = e `S.member` ls
      -}
      isE = e `S.member` ex
      {-
      cl | isR && not isE = "red"
         | isR            = "mediumblue"
         | isL            = "forestgreen"
         | otherwise      = "black"
       -}
      sh | isE       = "gold"
         | otherwise = "beige"
      sameMod = maybe True ((==) m) mm

shapeFor                     :: EntityType -> Shape
shapeFor Constructor{}       = Box3D
shapeFor RecordFunction{}    = Component
shapeFor ClassFunction{}     = DoubleOctagon
shapeFor DefaultInstance{}   = Octagon
shapeFor ClassInstance{}     = Octagon
shapeFor CollapsedData{}     = Box3D
shapeFor CollapsedClass{}    = DoubleOctagon
shapeFor CollapsedInstance{} = Octagon
shapeFor NormalEntity        = BoxShape

styleFor                 :: Maybe ModName -> ModName -> StyleItem
styleFor mm m@LocalMod{} = flip SItem [] . bool Bold Solid
                           $ maybe True ((==) m) mm
styleFor _  ExtMod{}     = SItem Dashed []
styleFor _  UnknownMod   = SItem Dotted []

callAttributes                        :: CallType -> Attributes
callAttributes NormalCall             = [ Color [ColorName "black"]]
callAttributes InstanceDeclaration    = [ Color [ColorName "navy"]
                                        , Dir NoDir
                                        ]
callAttributes DefaultInstDeclaration = [ Color [ColorName "turquoise"]
                                        , Dir NoDir
                                        ]
callAttributes RecordConstructor      = [ Color [ColorName "magenta"]
                                        , ArrowTail oDot
                                        , ArrowHead vee
                                        ]

callAttributes'              :: LEdge (Int, CallType) -> Attributes
callAttributes' (_,_,(n,ct)) = PenWidth (fromIntegral n)
                               : callAttributes ct

clustAttributes               :: EntClustType -> Attributes
clustAttributes (ClassDefn c) = [ Label . StrLabel $ "Class: " ++ c
                                , Style [SItem Filled [], SItem Rounded []]
                                , FillColor $ ColorName "rosybrown1"
                                ]
clustAttributes (DataDefn d)  = [ Label . StrLabel $ "Data: " ++ d
                                , Style [SItem Filled [], SItem Rounded []]
                                , FillColor $ ColorName "papayawhip"
                                ]
clustAttributes (ClassInst d) = [ Label . StrLabel $ "Instance for: " ++ d
                                , Style [SItem Filled [], SItem Rounded []]
                                , FillColor $ ColorName "slategray1"
                                ]
clustAttributes DefInst       = [ Label . StrLabel $ "Default Instance"
                                , Style [SItem Filled [], SItem Rounded []]
                                , FillColor $ ColorName "slategray1"
                                ]
clustAttributes (ModPath p)   = [ Label $ StrLabel p ]

clustAttributes' :: EntClustType -> [GlobalAttributes]
clustAttributes' = return . GraphAttrs . clustAttributes

modClustAttrs   :: ModName -> [GlobalAttributes]
modClustAttrs m = [GraphAttrs [ Label . StrLabel $ nameOfModule m
                              , Style [SItem Filled []]
                              , FillColor $ ColorName "wheat1"
                              ]
                  ]

-- -----------------------------------------------------------------------------

-- | Create a 'DotGraph' using a clustering function.
drawClusters           :: String -> (HSGraph -> HSClustGraph) -> HSData -> DotGraph Node
drawClusters gid cf dg = setID (Str gid)
                         $ graphvizClusters dg'
                                            gAttrs
                                            (const cAttr)
                                            nAttr
                                            callAttributes'
    where
      gAttrs = [] -- [GraphAttrs [Label $ StrLabel t]]
      cAttr = [GraphAttrs [ Style [SItem Filled []]
                          , FillColor $ ColorName "wheat1"
                          ]
              ]
      dg' = updateGraph (compactSame . cf . collapseStructures') dg
      rs = getRoots dg
      ls = getLeaves dg
      es = getWRoots dg
      nAttr = entityAttributes rs ls es True Nothing

-- -----------------------------------------------------------------------------

drawModules        :: String -> ModData -> DotGraph Node
drawModules gid dg = setID (Str gid)
                     $ graphvizClusters' dg
                                         gAttrs
                                         clusteredModule
                                         cID
                                         cAttr
                                         nAttr
                                         (const [])
    where
      cID s = bool (Just $ Str s) Nothing $ null s
      gAttrs = [] --[GraphAttrs [Label $ StrLabel t]]
      cAttr p = [GraphAttrs [Label $ StrLabel p]]
      rs = I.fromList $ applyAlg rootsOf' dg
      ls = I.fromList $ applyAlg leavesOf' dg
      es = I.fromList $ wantedRootNodes dg
      nAttr (n,m) = [ Label $ StrLabel m
                    , Color [ColorName $ mCol rs ls es n]
                    , Shape Tab
                    ]

mCol :: IntSet -> IntSet -> IntSet -> Node -> String
mCol rs ls es n
    | isR && not isE = "red"
    | isR            = "mediumblue"
    | isL            = "forestgreen"
    | otherwise      = "black"
    where
      isR = n `I.member` rs
      isL = n `I.member` ls
      isE = n `I.member` es
