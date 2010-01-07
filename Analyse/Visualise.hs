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
import Analyse.Colors(defaultNodeColor, clusterBackground)

import Data.Graph.Analysis hiding (Bold)
import Data.Graph.Inductive
import Data.GraphViz

import Data.Maybe(isJust, maybe)
import Data.List(find)
import qualified Data.Set as S

-- -----------------------------------------------------------------------------

-- | Create the nested 'DotGraph'.
drawGraph           :: String -> Maybe ModName -> HData' -> DotGraph Node
drawGraph gid mm dg = setID (Str gid)
                      $ graphvizClusters' (compactData dg')
                                          gAttrs
                                          toClust
                                          ctypeID
                                          clustAttributes'
                                          nAttr
                                          callAttributes'
    where
      dg' = origHData dg
      gAttrs = [nodeAttrs] -- [GraphAttrs [Label $ StrLabel t]]
      -- Possible clustering problem
      toClust = clusterEntity -- bool clusterEntity clusterEntityM' $ isJust mm
      nAttr = entityAttributes dg' (not $ isJust mm) mm

-- | One-module-per-cluster 'DotGraph'.
drawGraph'        :: String -> HData' -> DotGraph Node
drawGraph' gid dg = setID (Str gid)
                    $ graphvizClusters (compactData dg')
                                       gAttrs
                                       modClustAttrs
                                       nAttr
                                       callAttributes'
    where
      dg' = collapsedHData dg
      gAttrs = [nodeAttrs] -- [GraphAttrs [Label $ StrLabel t]]
      nAttr = entityAttributes dg' False Nothing

-- -----------------------------------------------------------------------------

-- | 'True' if add explicit module name to all entities, @'Just' m@ if
--   only one module, @'Nothing'@ if all.
entityAttributes :: GData n e -> Bool -> Maybe ModName
                    -> LNode Entity -> Attributes
entityAttributes hd a mm (n,(Ent m nm t))
    = [ Label $ StrLabel lbl
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
styleFor mm m@LocalMod{} = flip SItem [] . bool Solid Bold
                           $ maybe False ((==) m) mm
styleFor _  ExtMod{}     = SItem Dashed []
styleFor _  UnknownMod   = SItem Dotted []

callAttributes                        :: CallType -> Attributes
callAttributes NormalCall             = [ Color [X11Color Black]]
callAttributes InstanceDeclaration    = [ Color [X11Color Navy]
                                        , Dir NoDir
                                        ]
callAttributes DefaultInstDeclaration = [ Color [X11Color Turquoise]
                                        , Dir NoDir
                                        ]
callAttributes RecordConstructor      = [ Color [X11Color Magenta]
                                        , ArrowTail oDot
                                        , ArrowHead vee
                                        ]

callAttributes'              :: LEdge (Int, CallType) -> Attributes
callAttributes' (_,_,(n,ct)) = PenWidth (fromIntegral n)
                               : callAttributes ct

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
clustAttributes (ModPath p)     = [ Label $ StrLabel p ]

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
                         $ graphvizClusters (compactData dg')
                                            gAttrs
                                            (const cAttr)
                                            nAttr
                                            callAttributes'
    where
      dg' = mapData (updateGraph cf) $ collapsedHData dg
      gAttrs = [nodeAttrs] -- [GraphAttrs [Label $ StrLabel t]]
      cAttr = [GraphAttrs [ Style [SItem Filled []]
                          , FillColor clusterBackground
                          ]
              ]
      nAttr = entityAttributes dg' True Nothing

-- -----------------------------------------------------------------------------
-- Dealing with inter-module imports, etc.

drawModules        :: String -> MData -> DotGraph Node
drawModules gid md = setID (Str gid)
                     $ graphvizClusters' (graphData md)
                                         gAttrs
                                         clusteredModule
                                         cID
                                         cAttr
                                         nAttr
                                         (const [])
    where
      cID s = bool Nothing (Just $ Str s) $ (not . null) s
      gAttrs = [nodeAttrs] -- [GraphAttrs [Label $ StrLabel t]]
      cAttr p = [GraphAttrs [Label $ StrLabel p]]
      nAttr (n,m) = [ Label $ StrLabel m
                    , FillColor $ entCol md n
                    , Shape Tab
                    ]

-- -----------------------------------------------------------------------------

entCol     :: GData n e -> Node -> Color
entCol d n = maybe defaultNodeColor snd
             . find hasNode
             $ nodeCols d
  where
    hasNode = S.member n . fst

nodeAttrs :: GlobalAttributes
nodeAttrs = NodeAttrs [ Margin . PVal $ PointD 0.4 0.1
                      , Style [SItem Filled []]
                      ]
