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
module Analyse.Colors where

import Data.GraphViz.Attributes.Colors

inaccessibleColor :: Color
inaccessibleColor = X11Color Crimson

exportedRootColor :: Color
exportedRootColor = X11Color Gold

exportedInnerColor :: Color
exportedInnerColor = X11Color Goldenrod

implicitExportColor :: Color
implicitExportColor = X11Color Khaki

leafColor :: Color
leafColor = X11Color Cyan

defaultNodeColor :: Color
defaultNodeColor = X11Color Bisque

defaultEdgeColor :: Color
defaultEdgeColor = X11Color Black

cliqueColor :: Color
cliqueColor = X11Color Orange

cycleColor :: Color
cycleColor = X11Color DarkOrchid

chainColor :: Color
chainColor = X11Color Chartreuse

clusterBackground :: Color
clusterBackground = X11Color Lavender
