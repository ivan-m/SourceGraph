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
   Module      : Parsing.Types
   Description : Types for parsing Haskell code.
   Copyright   : (c) Ivan Lazar Miljenovic 2008
   License     : GPL-3 or later.
   Maintainer  : Ivan.Miljenovic@gmail.com

   Types for parsing Haskell modules.
 -}

module Parsing.Types where

import Data.Maybe
import qualified Data.Map as M
import Data.Map(Map)
import System.FilePath(addExtension)

type HaskellModules = Map ModuleName HaskellModule

data ModuleName = M (Maybe String) String
                  deriving (Eq, Ord)

type FunctionLookup = Map (Maybe String,String) Function

type FunctionCalls = Map Function [Function]

createLookup :: [Function] -> FunctionLookup
createLookup = M.fromList . map addKey
    where
      addKey f = ((qualdBy f, name f), f)

functionLookup      :: FunctionLookup -> Function -> Maybe Function
functionLookup fl f = M.lookup k fl
    where
      k = (qualdBy f, name f)

lookupFunctions    :: FunctionLookup -> [Function] -> [Function]
lookupFunctions fl = catMaybes . map (functionLookup fl)


unknownModule :: ModuleName
unknownModule = M Nothing "Module Not Found"



instance Show ModuleName where
    show (M Nothing m)    = m
    show (M (Just dir) m) = addExtension dir m


data HaskellModule = Hs { moduleName :: ModuleName
                        , imports :: [ModuleName]
                        , exports :: [Function]
                        , functions :: FunctionCalls
                        }

data HsImport = I { fromModule :: ModuleName
                  , qualAs     :: Maybe String
                  , importList :: [Function]
                  }

data Function = F { inModule :: ModuleName
                  , name     :: String
                  , qualdBy  :: Maybe String
                  }
                deriving (Eq, Ord)

instance Show Function where
    show f = name f

defFunc   :: String -> Function
defFunc f = F unknownModule f Nothing

defFunc'     :: String -> String -> Function
defFunc' q f = F unknownModule f (Just q)


setFuncModule     :: ModuleName -> Function -> Function
setFuncModule m f = f { inModule = m }

setFuncModules :: ModuleName -> [Function] -> [Function]
setFuncModules m = map (setFuncModule m)
