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
import System.FilePath(addExtension, splitExtension)

-- -----------------------------------------------------------------------------

-- | A high-level viewpoint of a Haskell module.
data HaskellModule = Hs { moduleName :: ModuleName
                        , imports :: [ModuleName]
                        , exports :: [Function]
                        , functions :: FunctionCalls
                        }

-- | A lookup-map of 'HaskellModule's.
type HaskellModules = Map ModuleName HaskellModule

-- | Create the 'HaskellModules' lookup map from a list of 'HaskellModule's.
createModuleMap :: [HaskellModule] -> HaskellModules
createModuleMap = M.fromList . map (\m -> (moduleName m, m))

-- -----------------------------------------------------------------------------

-- | The name of a module.  The 'Maybe' component refers to the possible path
--   of this module.
data ModuleName = M (Maybe String) String
                  deriving (Eq, Ord)

instance Show ModuleName where
    show (M Nothing m)    = m
    show (M (Just dir) m) = addExtension dir m

-- | Create the 'ModuleName' from its 'String' representation.
createModule :: String -> ModuleName
createModule m = case (splitExtension m) of
                   (m',"") -> M Nothing m'
                   (d,m')  -> M (Just d) m'

-- | A default module, used for when you haven't specified which module
--   something belongs to yet.
unknownModule :: ModuleName
unknownModule = M Nothing "Module Not Found"

-- -----------------------------------------------------------------------------

-- | The import list of a module.
data HsImport = I { fromModule :: ModuleName
                  -- | How the module was imported, if it actually was.
                  , qualAs     :: Maybe String
                  -- | The functions from this module that were imported.
                  , importList :: [Function]
                  }

-- -----------------------------------------------------------------------------

-- | Defines a function.
data Function = F { inModule :: ModuleName
                  , name     :: String
                  , qualdBy  :: Maybe String
                  }
                deriving (Eq, Ord)

instance Show Function where
    show f = addExtension (show $ inModule f) (name f)

-- | Create a default function with using 'unknownModule'.
defFunc   :: String -> Function
defFunc f = F unknownModule f Nothing

-- | Set the module of this function.
setFuncModule     :: ModuleName -> Function -> Function
setFuncModule m f = f { inModule = m }

-- | Set the module of these functions.
setFuncModules :: ModuleName -> [Function] -> [Function]
setFuncModules m = map (setFuncModule m)

-- | Defines a lookup map between the used qualifier and function name,
--   and the actual /unqualified/ function.
type FunctionLookup = Map (Maybe String,String) Function

-- | Create a 'FunctionLookup' map using the given functions.
createLookup :: [Function] -> FunctionLookup
createLookup = M.fromList . map addKey
    where
      addKey f = ((qualdBy f, name f), f)

-- | Try to lookup the given function.
functionLookup      :: FunctionLookup -> Function -> Maybe Function
functionLookup fl f = M.lookup k fl
    where
      k = (qualdBy f, name f)

-- | Lookup the given functions, returning only those that are in the
--   'FunctionLookup' map.
lookupFunctions    :: FunctionLookup -> [Function] -> [Function]
lookupFunctions fl = catMaybes . map (functionLookup fl)



type FunctionCalls = Map Function [Function]

