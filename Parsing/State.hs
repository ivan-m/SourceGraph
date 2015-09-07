{-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}

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
   Module      : Parsing.State
   Description : State Monad for parsing.
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : GPL-3 or later.
   Maintainer  : Ivan.Miljenovic@gmail.com

   Customised State Monad for parsing Haskell code.
 -}
module Parsing.State
    ( PState
    , runPState
    , get
    , put
    , getModules
    , getModuleNames
    , getLookup
    , getFutureParsedModule
    , getModuleName
    ) where

import Parsing.Types

import Control.Monad.RWS

#if !(MIN_VERSION_base (4,8,0))
import Control.Applicative (Applicative)
#endif

-- -----------------------------------------------------------------------------

runPState               :: ParsedModules -> ModuleNames
                           -> ParsedModule -> PState a -> ParsedModule
runPState hms mns pm st = pm'
    where
      -- Tying the knot
      el = internalLookup pm'
      mp = MD hms mns el pm'
      (pm', _) = execRWS (runPS st) mp pm

data ModuleData = MD { moduleLookup       :: ParsedModules
                     , modNmsLookup       :: ModuleNames
                     , entityLookup       :: EntityLookup
                     , futureParsedModule :: ParsedModule
                     }

type ModuleWrite = ()

newtype PState value
  = PS { runPS :: RWS ModuleData ModuleWrite ParsedModule value }
    -- Note: don't derive MonadReader, etc. as don't want anything
    -- outside this module to get the actual types used.
  deriving (Functor, Applicative, Monad, MonadState ParsedModule, MonadWriter ModuleWrite)

asks' :: (ModuleData -> a) -> PState a
asks' = PS . asks

getModules :: PState ParsedModules
getModules = asks' moduleLookup

getModuleNames :: PState ModuleNames
getModuleNames = asks' modNmsLookup

getLookup :: PState EntityLookup
getLookup = asks' entityLookup

getFutureParsedModule :: PState ParsedModule
getFutureParsedModule = asks' futureParsedModule

getModuleName :: PState ModName
getModuleName = gets moduleName
