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

import Parsing.Types
import Analyse.GraphRepr
import Analyse.Utils
import Analyse.Colors

import Data.Graph.Analysis hiding (Bold)
import Data.GraphViz

import Data.Maybe(isNothing)
import Data.List(find)
import qualified Data.Set as S

-- -----------------------------------------------------------------------------

-- | Create the nested 'DotGraph'.
drawGraph           :: String -> Maybe ModName -> HData' -> DotGraph Node
drawGraph gid mm dg = setID (Str gid)
                      . graphviz params $ compactData dg'

    where
      params = Params True gAttrs toClust ctypeID clustAttributes' nAttr eAttr
      dg' = origHData dg
      gAttrs = [nodeAttrs] -- [GraphAttrs [toLabel t]]
      -- Possible clustering problem
      toClust = clusterEntity -- bool clusterEntity clusterEntityM' $ isJust mm
      nAttr = entityAttributes dg' (isNothing mm) mm
      eAttr = callAttributes' dg'

-- | One-module-per-cluster 'DotGraph'.
drawGraph'        :: String -> HData' -> DotGraph Node
drawGraph' gid dg = setID (Str gid)
                    . graphvizClusters params $ compactData dg'
    where
      params = Params True gAttrs N (const Nothing) modClustAttrs nAttr eAttr
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
      , Shape $ shapeFor t
      -- , Color [ColorName cl]
      , FillColor $ entCol hd n
        -- Have to re-set Filled because setting a new Style seems to
        -- override global Style.
      , Style [SItem Filled [], styleFor mm m]
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

styleFor                 :: Maybe ModName -> ModName -> StyleItem
styleFor mm m@LocalMod{} = flip SItem [] . bool Solid Bold
                           $ maybe False (m==) mm
styleFor _  ExtMod{}     = SItem Dashed []
styleFor _  UnknownMod   = SItem Dotted []

callAttributes                             :: GData n e -> Edge -> CallType
                                              -> Attributes
callAttributes hd e NormalCall             = [ Color [edgeCol hd e]]
callAttributes _  _ InstanceDeclaration    = [ Color [X11Color Navy]
                                             , Dir NoDir
                                             ]
callAttributes _  _ DefaultInstDeclaration = [ Color [X11Color Turquoise]
                                             , Dir NoDir
                                             ]
callAttributes _  _ RecordConstructor      = [ Color [X11Color Magenta]
                                             , ArrowTail oDot
                                             , ArrowHead vee
                                             ]

callAttributes'                 :: GData n e -> LEdge (Int, CallType)
                                   -> Attributes
callAttributes' hd (f,t,(n,ct)) = PenWidth (log (fromIntegral n) + 1)
                                  : callAttributes hd (f,t) ct

clustAttributes                 :: EntClustType -> Attributes
clustAttributes (ClassDefn c)   = [ Label . StrLabel $ "Class: " ++ c
                                  , Style [SItem Filled [], SItem Rounded []]
                                  , FillColor $ X11Color RosyBrown1
                                  ]
clustAttributes (DataDefn d)    = [ Label . StrLabel $ "Data: " ++ d
                                  , Style [SItem Filled [], SItem Rounded []]
                                  , FillColor $ X11Color PapayaWhip
                                  ]
clustAttributes (ClassInst _ d) = [ Label . StrLabel $ "Instance for: " ++ d
                                , Style [SItem Filled [], SItem Rounded []]
                                , FillColor $ X11Color SlateGray1
                                ]
clustAttributes DefInst{}       = [ Label . StrLabel $ "Default Instance"
                                  , Style [SItem Filled [], SItem Rounded []]
                                  , FillColor $ X11Color SlateGray1
                                  ]
clustAttributes (ModPath p)     = [ toLabel p ]

clustAttributes' :: EntClustType -> [GlobalAttributes]
clustAttributes' = return . GraphAttrs . clustAttributes

modClustAttrs   :: ModName -> [GlobalAttributes]
modClustAttrs m = [GraphAttrs [ Label . StrLabel $ nameOfModule m
                              , Style [SItem Filled []]
                              , FillColor clusterBackground
                              ]
                  ]

-- -----------------------------------------------------------------------------

-- | Create a 'DotGraph' using a clustering function.  The clustering
--   function shouldn't touch the the actual 'Node' values.
drawClusters           :: String -> (HSGraph -> HSClustGraph)
                          -> HData' -> DotGraph Node
drawClusters gid cf dg = setID (Str gid)
                         . graphvizClusters params $ compactData dg'
    where
      params = blankParams { globalAttributes = gAttrs
                           , fmtCluster       = const cAttr
                           , fmtNode          = nAttr
                           , fmtEdge          = eAttr
                           }
      dg' = mapData' cf $ collapsedHData dg
      gAttrs = [nodeAttrs] -- [GraphAttrs [toLabel t]]
      cAttr = [GraphAttrs [ Style [SItem Filled []]
                          , FillColor clusterBackground
                          ]
              ]
      nAttr = entityAttributes dg' True Nothing
      eAttr = callAttributes' dg'

drawLevels           :: String -> Maybe ModName -> HData' -> DotGraph Node
drawLevels gid mm hd = setID (Str gid)
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
      atts t = [GraphAttrs [ Label (StrLabel t)
                           , Style [SItem Filled []]
                           , FillColor clusterBackground
                           ]
               ]

-- -----------------------------------------------------------------------------
-- Dealing with inter-module imports, etc.

drawModules        :: String -> MData -> DotGraph Node
drawModules gid md = setID (Str gid)
                     . graphviz params $ graphData md
    where
      params = Params True gAttrs clusteredModule cID cAttr nAttr eAttr
      cID (_,s) = bool Nothing (Just $ Str s) $ (not . null) s
      gAttrs = [nodeAttrs] -- [GraphAttrs [toLabel t]]
      cAttr dp = [GraphAttrs $ directoryAttributes dp]
      nAttr (n,m) = [ toLabel m
                    , FillColor $ entCol md n
                    , Shape Tab
                    ]
      eAttr le = [Color [edgeCol md $ edge le]]

directoryAttributes       :: (Depth, String) -> Attributes
directoryAttributes (d,n) = col : [Style [SItem Filled []]
                                  , Label (StrLabel n)
                                  ]
  where
    col = bool (FillColor noBackground) (FillColor clusterBackground)
          $ d `mod` 2 == 0

-- -----------------------------------------------------------------------------

entCol     :: GData n e -> Node -> Color
entCol d n = maybe defaultNodeColor snd
             . find hasNode
             $ nodeCols d
  where
    hasNode = S.member n . fst

nodeAttrs :: GlobalAttributes
nodeAttrs = NodeAttrs [ Margin . PVal $ createPoint 0.4 0.1
                      , Style [SItem Filled []]
                      ]

edgeCol     :: GData n e -> Edge -> Color
edgeCol d e = maybe defaultEdgeColor snd
              . find hasEdge
              $ edgeCols d
  where
    hasEdge = S.member e . fst
