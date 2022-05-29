{-# LANGUAGE CPP #-}
#if !defined(MIN_VERSION_Cabal)
# define MIN_VERSION_Cabal(a,b,c) 0
#endif

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
   Module      : CabalInfo
   Description : Obtain information from a .cabal file.
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : GPL-3 or later.
   Maintainer  : Ivan.Miljenovic@gmail.com

   Used to parse and obtain information from the provided Cabal file.
 -}
module Language.Haskell.SourceGraph.CabalInfo(parseCabal) where

import Distribution.Compiler                         (CompilerInfo)
import Distribution.ModuleName                       (toFilePath)
import Distribution.Package
import Distribution.PackageDescription               hiding (author)
import Distribution.PackageDescription.Configuration
#if MIN_VERSION_Cabal(2,0,0)
import Distribution.PackageDescription.Parsec
#else
import Distribution.PackageDescription.Parse
#endif
import Distribution.Simple.Compiler                  (compilerInfo)
import Distribution.Simple.GHC                       (configure)
import Distribution.Simple.Program                   (defaultProgramDb)
import Distribution.System                           (buildPlatform)
import Distribution.Verbosity                        (silent)

import Control.Exception (SomeException (..), try)
import Control.Monad     (liftM)
import Data.List         (nub)
import Data.Maybe        (fromJust, isJust)
import System.FilePath   (dropExtension)
import Distribution.Types.ComponentRequestedSpec (defaultComponentRequestedSpec)

-- -----------------------------------------------------------------------------

emptyFlagAssignment :: FlagAssignment
#if MIN_VERSION_Cabal(2,0,0)
emptyFlagAssignment = mkFlagAssignment []
#else
emptyFlagAssignment = []
#endif

#if MIN_VERSION_Cabal(2,0,0)
readDescription = readGenericPackageDescription
#else
readDescription = readPackageDescription
#endif

ghcID :: IO CompilerInfo
ghcID = liftM (compilerInfo . getCompiler)
        $ configure silent Nothing Nothing defaultProgramDb
  where
    getCompiler (comp,_mplat,_progconfig) = comp

parseCabal    :: FilePath -> IO (Maybe (String, [FilePath]))
parseCabal fp = do cID <- ghcID
                   liftM (parseDesc cID) $ getDesc fp
    where
      -- Need to specify the Exception type
      getDesc :: FilePath -> IO (Either SomeException GenericPackageDescription)
      getDesc = try . readDescription silent
      parseDesc cID = fmap parse . compactEithers . fmap (unGeneric cID)
      unGeneric cID = fmap fst
                      . finalizePD emptyFlagAssignment -- flags, use later
                                   defaultComponentRequestedSpec
                                   (const True) -- ignore
                                   -- deps
                                   buildPlatform
                                   cID
                                   []
      parse pd = (nm, exps)
          where
            nm = pName . pkgName $ package pd
            pName nm' = unPackageName nm'
            exes = filter (buildable . buildInfo) $ executables pd
            lib = library pd
            moduleNames = map toFilePath
            exps | not $ null exes  = nub $ map (dropExtension . modulePath) exes
                 | isJust lib       = moduleNames . exposedModules $ fromJust lib
                 | otherwise        = error "No exposed modules"

compactEithers :: Either a (Either b c) -> Maybe c
compactEithers (Right (Right c)) = Just c
compactEithers _                 = Nothing
