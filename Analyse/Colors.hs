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

import Data.GraphViz.Attributes

inaccessibleColor :: X11Color
inaccessibleColor = Crimson

exportedRootColor :: X11Color
exportedRootColor = Gold

exportedInnerColor :: X11Color
exportedInnerColor = Goldenrod

implicitExportColor :: X11Color
implicitExportColor = Khaki

leafColor :: X11Color
leafColor = Cyan

defaultNodeColor :: X11Color
defaultNodeColor = Bisque

defaultEdgeColor :: X11Color
defaultEdgeColor = Black

cliqueColor :: X11Color
cliqueColor = Orange

cycleColor :: X11Color
cycleColor = DarkOrchid

chainColor :: X11Color
chainColor = Chartreuse

clusterBackground :: X11Color
clusterBackground = Lavender

noBackground :: X11Color
noBackground = Snow
