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
    , getModules
    , getModuleNames
    , getLookup
    , getParsedModule
    , getFutureParsedModule
    , getModuleName
    , putParsedModule
    ) where

import Parsing.Types

-- -----------------------------------------------------------------------------

runPState               :: ParsedModules -> ModuleNames
                           -> ParsedModule -> PState a -> ParsedModule
runPState hms mns pm st = pm'
    where
      -- Tying the knot
      el = internalLookup pm'
      mp = MP hms mns el pm pm'
      pm' = parsedModule $ execState st mp

data ModuleParsing = MP { moduleLookup :: ParsedModules
                        , modNmsLookup :: ModuleNames
                        , entityLookup :: EntityLookup
                        , parsedModule :: ParsedModule
                        , futureParsedModule :: ParsedModule
                        }

newtype PState value
    = PS { runState :: ModuleParsing -> (value, ModuleParsing) }

instance Monad PState where
    return v = PS (\ps -> (v,ps))

    x >>= f = PS $ \ps -> let (r, ps') = runState x ps
                          in runState (f r) ps'


getModules :: PState ParsedModules
getModules = gets moduleLookup

getModuleNames :: PState ModuleNames
getModuleNames = gets modNmsLookup

getLookup :: PState EntityLookup
getLookup = gets entityLookup

getParsedModule :: PState ParsedModule
getParsedModule = gets parsedModule

getFutureParsedModule :: PState ParsedModule
getFutureParsedModule = gets futureParsedModule

getModuleName :: PState ModName
getModuleName = gets (moduleName . parsedModule)

putParsedModule    :: ParsedModule -> PState ()
putParsedModule pm = modify (\ s -> s { parsedModule = pm } )


get :: PState ModuleParsing
get = PS (\s -> (s,s))

put   :: ModuleParsing -> PState ()
put s = PS (const ((), s))

execState   :: PState value -> ModuleParsing -> ModuleParsing
execState s = snd . runState s

modify   :: (ModuleParsing -> ModuleParsing) -> PState ()
modify f = do s <- get
              put (f s)

gets   :: (ModuleParsing -> value) -> PState value
gets f = do s <- get
            return (f s)
