{-
Copyright (C) 2008 Ivan Lazar Miljenovic <Ivan.Miljenovic@gmail.com>

This program is free software; you can redistribute it and/or modify
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
   Module      : Main
   Description : Analyse source code as a graph.
   Copyright   : (c) Ivan Lazar Miljenovic 2008
   License     : GPL-3 or later.
   Maintainer  : Ivan.Miljenovic@gmail.com

   The executable file for the /SourceGraph/ programme.

   This was written as part of my mathematics honours thesis,
   /Graph-Theoretic Analysis of the Relationships in Discrete Data/.
 -}
module Main where

import Parsing
import Analyse

import Data.Graph.Analysis
import Data.Graph.Analysis.Reporting.Pandoc

import Distribution.Package
import Distribution.PackageDescription hiding (author)
import Distribution.Verbosity

import Data.Char
import Data.List
import Data.Maybe
import System.IO
import System.Directory
import System.FilePath
import System.Random
import System.Environment
import Control.Monad
import Control.Exception

main :: IO ()
main = do input <- getArgs
          let mcbl = getCabalFile input
          case mcbl of
            Nothing
                -> putErrLn "Please pass in a .cabal file"
            Just cbl
                -> do pcbl <- parseCabal cbl
                      case pcbl of
                        Nothing
                            -> putErrLn $ unwords [cbl,"is unparseable"]
                        Just (nm,exps)
                            -> do let dir = dropFileName cbl
                                  dir' <- if null dir
                                            then getCurrentDirectory
                                            else return dir
                                  hms <- parseFilesFrom dir'
                                  analyseCode dir nm exps hms

programName :: String
programName = "SourceGraph"

programVersion :: String
programVersion = "0.2"

putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr

-- -----------------------------------------------------------------------------

parseCabal    :: FilePath -> IO (Maybe (String, [ModuleName]))
parseCabal fp = do gpd <- try $ readPackageDescription silent fp
                   case gpd of
                     (Right gpd') -> return (Just $ parse gpd')
                     (Left _)     -> return Nothing
    where
      parse pd = (nm, exp')
          where
            cbl = packageDescription pd
            nm = pkgName $ package cbl
            cexes :: [Executable]
            cexes = map (condTreeData .snd) $ condExecutables pd
            exes = executables cbl
            clib = condLibrary pd
            lib = library cbl
            exp | not $ null cexes = nub $ map (dropExtension . modulePath) cexes
                | not $ null exes  = nub . map dropExtension $ exeModules cbl
                | isJust clib      = exposedModules . condTreeData
                                     $ fromJust clib
                | isJust lib       = exposedModules $ fromJust lib
                | otherwise        = error "No exposed modules"
            exp' = map createModule exp

getCabalFile :: [FilePath] -> Maybe FilePath
getCabalFile = listToMaybe . filter isCabalFile
    where
      isCabalFile f  = (takeExtension f) == (extSeparator : "cabal")

-- -----------------------------------------------------------------------------

-- | Recursively parse all files from this directory
parseFilesFrom    :: FilePath -> IO HaskellModules
parseFilesFrom fp = do files <- getHaskellFilesFrom fp
                       cnts <- readFiles files
                       return $ parseHaskell cnts

-- -----------------------------------------------------------------------------

-- Reading in the files.

-- | Recursively find all Haskell source files from the current directory.
getHaskellFilesFrom :: FilePath -> IO [FilePath]
getHaskellFilesFrom fp
    = do isDir <- doesDirectoryExist fp -- Ensure it's a directory.
         if isDir
            then do r <- try getFilesIn -- Ensure we can read the directory.
                    case r of
                      (Right fs) -> return fs
                      (Left _)   -> return []
            else return []
    where
      -- Filter out "." and ".." to stop infinite recursion.
      nonTrivialContents :: IO [FilePath]
      nonTrivialContents = do contents <- getDirectoryContents fp
                              let contents' = filter (not . isTrivial) contents
                              return $ map (fp </>) contents'
      getFilesIn :: IO [FilePath]
      getFilesIn = do contents <- nonTrivialContents
                      (dirs,files) <- partitionM doesDirectoryExist contents
                      let hFiles = filter isHaskellFile files
                      recursiveFiles <- concatMapM getHaskellFilesFrom dirs
                      return (hFiles ++ recursiveFiles)

-- | Determine if this is the path of a Haskell file.
isHaskellFile   :: FilePath -> Bool
isHaskellFile f = (takeExtension f) `elem` haskellExtensions

haskellExtensions :: [FilePath]
haskellExtensions = map (extSeparator :) ["hs","lhs"]

-- | Read in all the files that it can.
readFiles :: [FilePath] -> IO [FileContents]
readFiles = liftM catMaybes . mapM readFileContents

-- | Try to read the given file.
readFileContents   :: FilePath -> IO (Maybe FileContents)
readFileContents f = do cnts <- try $ readFile f
                        case cnts of
                          (Right str) -> return $ Just (f,str)
                          (Left _)    -> return Nothing

-- | A version of 'concatMap' for use in monads.
concatMapM   :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f = liftM concat . mapM f

-- | A version of 'partition' for use in monads.
partitionM      :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = return ([],[])
partitionM p (x:xs) = do ~(ts,fs) <- partitionM p xs
                         matches <- p x
                         if matches
                            then return (x:ts,fs)
                            else return (ts,x:fs)

-- | Trivial paths are the current directory, the parent directory and
--   such directories
isTrivial          :: FilePath -> Bool
isTrivial "."      = True
isTrivial ".."     = True
isTrivial "_darcs" = True
isTrivial "dist"   = True
isTrivial "Pandoc.hs" = True
isTrivial f | isSetup f = True
isTrivial _        = False

lowerCase :: String -> String
lowerCase = map toLower

isSetup   :: String -> Bool
isSetup f = lowerCase f `elem` (map ("setup" <.>) haskellExtensions)

-- -----------------------------------------------------------------------------

analyseCode                :: FilePath -> String -> [ModuleName]
                           -> HaskellModules -> IO ()
analyseCode fp nm exps hms = do d <- today
                                g <- newStdGen
                                let dc = doc d g
                                out <- createDocument pandocHtml dc
                                case out of
                                  Just fp -> success fp
                                  Nothing -> failure
    where
      doc d g = Doc { rootDirectory = rt
                    , fileFront     = nm
                    , title         = t
                    , author        = a
                    , date          = d
                    , content       = c g
                    }
      rt = fp </> programName
      sv s v = s ++ " (version " ++ v ++ ")"
      t = Grouping [Text "Analysis of", Emphasis $ Text nm]
      a = unwords [ "Analysed by", sv programName programVersion
                  , "using", sv "Graphalyze" version]
      c g = analyse g exps hms
      success fp = putStrLn $ unwords ["Report generated at:",fp]
      failure = putErrLn "Unable to generate report"
