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
   Module      : Analyse.Visualise
   Description : Visualisation functions.
   Copyright   : (c) Ivan Lazar Miljenovic 2010
   License     : GPL-3 or later.
   Maintainer  : Ivan.Miljenovic@gmail.com

   Utility functions and types for analysis.
 -}
module Analyse.Visualise where

import Analyse.Colors
import Analyse.GraphRepr
import Analyse.Utils
import Parsing.Types

import Data.Graph.Analysis               hiding (Bold)
import Data.GraphViz
import Data.GraphViz.Attributes.Complete (Attribute (Margin), DPoint (PVal),
                                          createPoint)

import           Data.List  (find)
import           Data.Maybe (isNothing)
import qualified Data.Set   as S

-- -----------------------------------------------------------------------------

-- | Create the nested 'DotGraph'.
drawGraph           :: String -> Maybe ModName -> HData' -> DotGraph Node
drawGraph gid mm dg = setID (toGraphID gid)
                      . graphviz params $ compactData dg'

    where
      params = Params True gAttrs toClust isClust ctypeID clustAttributes' nAttr eAttr
      dg' = origHData dg
      gAttrs = [nodeAttrs] -- [GraphAttrs [toLabel t]]
      -- Possible clustering problem
      toClust = clusterEntity -- bool clusterEntity clusterEntityM' $ isJust mm
      isClust = const True
      nAttr = entityAttributes dg' (isNothing mm) mm
      eAttr = callAttributes' dg'

-- | One-module-per-cluster 'DotGraph'.
drawGraph'        :: String -> HData' -> DotGraph Node
drawGraph' gid dg = setID (toGraphID gid)
                    . graphvizClusters params $ compactData dg'
    where
      params = Params True gAttrs N undefined undefined modClustAttrs nAttr eAttr
      dg' = collapsedHData dg
      gAttrs = [nodeAttrs] -- [GraphAttrs [toLabel t]]
      nAttr = entityAttributes dg' False Nothing
      eAttr = callAttributes' dg'

-- -----------------------------------------------------------------------------

-- | 'True' if add explicit module name to all entities, @'Just' m@ if
--   only one module, @'Nothing'@ if all.
entityAttributes :: GData n e -> Bool -> Maybe ModName
                    -> LNode Entity -> Attributes
entityAttributes hd a mm (n,Ent m nm t)
    = [ toLabel lbl
      , shape $ shapeFor t
      -- , Color ColorName cl
      , fillColor $ entCol hd n
        -- Have to re-set Filled because setting a new Style seems to
        -- override global Style.
      , styles [filled, styleFor mm m]
      ]
    where
      lbl = bool nm (nameOfModule m ++ "\\n" ++ nm)
            $ not sameMod || a
      sameMod = maybe True (m==) mm

shapeFor                     :: EntityType -> Shape
shapeFor Constructor{}       = Box3D
shapeFor RecordFunction{}    = Component
shapeFor ClassMethod{}       = DoubleOctagon
shapeFor DefaultInstance{}   = Octagon
shapeFor ClassInstance{}     = Octagon
shapeFor CollapsedData{}     = Box3D
shapeFor CollapsedClass{}    = DoubleOctagon
shapeFor CollapsedInstance{} = Octagon
shapeFor NormalEntity        = BoxShape

styleFor                 :: Maybe ModName -> ModName -> Style
styleFor mm m@LocalMod{} = bool solid bold $ maybe False (m==) mm
styleFor _  ExtMod{}     = dashed
styleFor _  UnknownMod   = dotted

callAttributes                             :: GData n e -> Edge -> CallType
                                              -> Attributes
callAttributes hd e NormalCall             = [ color $ edgeCol hd e]
callAttributes _  _ InstanceDeclaration    = [ color Navy
                                             , edgeEnds NoDir
                                             ]
callAttributes _  _ DefaultInstDeclaration = [ color Turquoise
                                             , edgeEnds NoDir
                                             ]
callAttributes _  _ RecordConstructor      = [ color Magenta
                                             , arrowFrom oDot
                                             , arrowTo vee
                                             ]

callAttributes'                 :: GData n e -> LEdge (Int, CallType)
                                   -> Attributes
callAttributes' hd (f,t,(n,ct)) = penWidth (log (fromIntegral n) + 1)
                                  : callAttributes hd (f,t) ct

clustAttributes                 :: EntClustType -> Attributes
clustAttributes (ClassDefn c)   = [ toLabel $ "Class: " ++ c
                                  , styles [filled, rounded]
                                  , fillColor RosyBrown1
                                  ]
clustAttributes (DataDefn d)    = [ toLabel $ "Data: " ++ d
                                  , styles [filled, rounded]
                                  , fillColor PapayaWhip
                                  ]
clustAttributes (ClassInst _ d) = [ toLabel $ "Instance for: " ++ d
                                  , styles [filled, rounded]
                                  , fillColor SlateGray1
                                  ]
clustAttributes DefInst{}       = [ toLabel $ "Default Instance"
                                  , styles [filled, rounded]
                                  , fillColor SlateGray1
                                  ]
clustAttributes (ModPath p)     = [ toLabel p ]

clustAttributes' :: EntClustType -> [GlobalAttributes]
clustAttributes' = return . GraphAttrs . clustAttributes

modClustAttrs   :: ModName -> [GlobalAttributes]
modClustAttrs m = [GraphAttrs [ toLabel $ nameOfModule m
                              , style filled
                              , fillColor clusterBackground
                              ]
                  ]

-- -----------------------------------------------------------------------------

-- | Create a 'DotGraph' using a clustering function.  The clustering
--   function shouldn't touch the the actual 'Node' values.
drawClusters           :: String -> (HSGraph -> HSClustGraph)
                          -> HData' -> DotGraph Node
drawClusters gid cf dg = setID (toGraphID gid)
                         . graphvizClusters params $ compactData dg'
    where
      params = blankParams { globalAttributes = gAttrs
                           , fmtCluster       = const cAttr
                           , fmtNode          = nAttr
                           , fmtEdge          = eAttr
                           }
      dg' = mapData' cf $ collapsedHData dg
      gAttrs = [nodeAttrs] -- [GraphAttrs [toLabel t]]
      cAttr = [GraphAttrs [ style filled
                          , fillColor clusterBackground
                          ]
              ]
      nAttr = entityAttributes dg' True Nothing
      eAttr = callAttributes' dg'

drawLevels           :: String -> Maybe ModName -> HData' -> DotGraph Node
drawLevels gid mm hd = setID (toGraphID gid)
                       $ graphvizClusters params dg'
  where
    params = blankParams { globalAttributes = gAttrs
                         , fmtCluster       = levelAttr
                         , fmtNode          = nAttr
                         , fmtEdge          = eAttr
                         }
    hd' = collapsedHData hd
    vs = collapsedVirts hd
    dg = compactData hd'
    wrs = wantedRootNodes dg ++ S.toList (implicitExports vs dg)
    dg' = updateGraph (levelGraphFrom wrs) dg
    gAttrs = [nodeAttrs] -- [GraphAttrs [toLabel t]]
    nAttr = entityAttributes hd' (isNothing mm) mm
    eAttr = callAttributes' hd'

levelAttr :: Int -> [GlobalAttributes]
levelAttr l
  | l < minLevel  = atts "Inaccessible entities"
  | l == minLevel = atts "Exported root entities"
  | otherwise     = atts $ "Level = " ++ show l
    where
      atts t = [GraphAttrs [ toLabel t
                           , style filled
                           , fillColor clusterBackground
                           ]
               ]

-- -----------------------------------------------------------------------------
-- Dealing with inter-module imports, etc.

drawModules        :: String -> MData -> DotGraph Node
drawModules gid md = setID (toGraphID gid)
                     . graphviz params $ graphData md
    where
      params = Params True gAttrs clusteredModule isClust cID cAttr nAttr eAttr
      isClust = const True
      cID (_,s) = bool undefined (toGraphID s) $ (not . null) s
      gAttrs = [nodeAttrs] -- [GraphAttrs [toLabel t]]
      cAttr dp = [GraphAttrs $ directoryAttributes dp]
      nAttr (n,m) = [ toLabel m
                    , fillColor $ entCol md n
                    , shape Tab
                    ]
      eAttr le = [color . edgeCol md $ edge le]

directoryAttributes       :: (Depth, String) -> Attributes
directoryAttributes (d,n) = col : [ style filled
                                  , toLabel n
                                  ]
  where
    col = bool (fillColor noBackground) (fillColor clusterBackground)
          $ d `mod` 2 == 0

-- -----------------------------------------------------------------------------

entCol     :: GData n e -> Node -> X11Color
entCol d n = maybe defaultNodeColor snd
             . find hasNode
             $ nodeCols d
  where
    hasNode = S.member n . fst

nodeAttrs :: GlobalAttributes
nodeAttrs = NodeAttrs [ Margin . PVal $ createPoint 0.4 0.1
                      , style filled
                      ]

edgeCol     :: GData n e -> Edge -> X11Color
edgeCol d e = maybe defaultEdgeColor snd
              . find hasE
              $ edgeCols d
  where
    hasE = S.member e . fst
