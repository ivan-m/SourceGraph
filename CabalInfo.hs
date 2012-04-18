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
module CabalInfo(parseCabal) where

import Distribution.Package
import Distribution.PackageDescription hiding (author)
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.Configuration
import Distribution.Compiler(CompilerId)
import Distribution.ModuleName(toFilePath)
import Distribution.Verbosity(silent)
import Distribution.Simple.Compiler(compilerId)
import Distribution.Simple.GHC(configure)
import Distribution.Simple.Program(defaultProgramConfiguration)
import Distribution.System(buildPlatform)

import Data.List(nub)
import Data.Maybe(isJust, fromJust)
import Control.Exception(SomeException(..), try)
import Control.Monad(liftM)
import System.FilePath(dropExtension)

-- -----------------------------------------------------------------------------

ghcID :: IO CompilerId
ghcID = liftM (compilerId . fst)
        $ configure silent Nothing Nothing defaultProgramConfiguration

parseCabal    :: FilePath -> IO (Maybe (String, [FilePath]))
parseCabal fp = do cID <- ghcID
                   liftM (parseDesc cID) $ getDesc fp
    where
      -- Need to specify the Exception type
      getDesc :: FilePath -> IO (Either SomeException GenericPackageDescription)
      getDesc = try . readPackageDescription silent
      parseDesc cID = fmap parse . compactEithers . fmap (unGeneric cID)
      unGeneric cID = fmap fst
                      . finalizePackageDescription [] -- flags, use later
                                                   (const True) -- ignore
                                                                -- deps
                                                   buildPlatform
                                                   cID
                                                   []
      parse pd = (nm, exps)
          where
            nm = pName . pkgName $ package pd
            pName (PackageName nm') = nm'
            exes = filter (buildable . buildInfo) $ executables pd
            lib = library pd
            moduleNames = map toFilePath
            exps | not $ null exes  = nub $ map (dropExtension . modulePath) exes
                 | isJust lib       = moduleNames . exposedModules $ fromJust lib
                 | otherwise        = error "No exposed modules"

compactEithers :: Either a (Either b c) -> Maybe c
compactEithers (Right (Right c)) = Just c
compactEithers _                 = Nothing
