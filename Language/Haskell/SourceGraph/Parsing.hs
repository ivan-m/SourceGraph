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
   Module      : Parsing
   Description : Parse the given Haskell modules.
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : GPL-3 or later.
   Maintainer  : Ivan.Miljenovic@gmail.com

   Parse the given Haskell modules.
 -}
module Language.Haskell.SourceGraph.Parsing where

import Language.Haskell.SourceGraph.Parsing.Types
import Language.Haskell.SourceGraph.Parsing.ParseModule

import Language.Haskell.Exts(parseFileContentsWithMode)
import Language.Haskell.Exts.Parser( ParseMode(..)
                                   , ParseResult(..)
                                   , defaultParseMode)
import Language.Haskell.Exts.Syntax(Module)
import Language.Haskell.Exts.SrcLoc

import Data.Either(partitionEithers)

import Control.Exception
import Control.Monad
import Data.Maybe
import Data.Char

import System.Directory( getCurrentDirectory
                       , doesDirectoryExist
                       , doesFileExist
                       , getDirectoryContents)
import System.FilePath( dropFileName
                      , takeExtension
                      , isPathSeparator
                      , (</>)
                      , (<.>))

type FileContents = (FilePath,String)

-- | Parse all the files and return the map.
--   This uses laziness to evaluate the 'HaskellModules' result
--   whilst also using it to parse all the modules to create it.
parseHaskell    :: [FileContents] -> ([FilePath],ParsedModules)
parseHaskell fc = (failed,hms)
    where
      (failed,ms) = parseFiles fc
      hms = createModuleMap hss
      hss = map (parseModule hms) ms

-- | Attempt to parse an individual file.
parseFile       :: FileContents -> Either FilePath (Module SrcSpanInfo)
parseFile (p,f) = case (parseFileContentsWithMode mode f) of
                    (ParseOk hs) -> Right hs
                    _            -> Left p
    where
      mode = defaultParseMode { parseFilename = p
                              , fixities = Nothing
                              }

-- | Parse all the files that you can.
parseFiles :: [FileContents] -> ([FilePath],[Module SrcSpanInfo])
parseFiles = partitionEithers . map parseFile

-- | Read in all the files that it can.
readFiles :: [FilePath] -> IO [FileContents]
readFiles = liftM catMaybes . mapM readFileContents

-- | Try to read the given file.
readFileContents   :: FilePath -> IO (Maybe FileContents)
readFileContents f = do cnts <- try $ readFile f
                        case cnts of
                          (Right str)            -> return $ Just (f,str)
                          (Left SomeException{}) -> return Nothing

-- -----------------------------------------------------------------------------

-- | Recursively parse all files from this directory
parseFilesFrom    :: FilePath -> IO ([FilePath],ParsedModules)
parseFilesFrom fp = parseHaskellFiles =<< getHaskellFilesFrom fp

parseHaskellFiles :: [FilePath] -> IO ([FilePath],ParsedModules)
parseHaskellFiles = liftM parseHaskell . readFiles

-- Reading in the files.

-- | Recursively find all Haskell source files from the current directory.
getHaskellFilesFrom :: FilePath -> IO [FilePath]
getHaskellFilesFrom fp
    = do isDir <- doesDirectoryExist fp -- Ensure it's a directory.
         if isDir
            then do r <- try getFilesIn -- Ensure we can read the directory.
                    case r of
                      (Right fs)             -> return fs
                      (Left SomeException{}) -> return []
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

-- | A version of 'concatMap' for use in monads.
concatMapM   :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f = liftM concat . mapM f

-- | Determine if this is the path of a Haskell file.
isHaskellFile    :: FilePath -> Bool
isHaskellFile fp = any (`hasExt` fp) haskellExtensions

hasExt     :: String -> FilePath -> Bool
hasExt ext = (==) ext . drop 1 . takeExtension

fpToModule :: FilePath -> ModName
fpToModule = createModule . map pSep
    where
      pSep c
          | isPathSeparator c = moduleSep
          | otherwise         = c

-- -----------------------------------------------------------------------------

haskellExtensions :: [FilePath]
haskellExtensions = ["hs","lhs"]

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
isTrivial               :: FilePath -> Bool
isTrivial "."           = True
isTrivial ".."          = True
isTrivial "_darcs"      = True
isTrivial "dist"        = True
isTrivial "HLInt.hs"    = True
isTrivial f | isSetup f = True
isTrivial _             = False

lowerCase :: String -> String
lowerCase = map toLower

isSetup   :: String -> Bool
isSetup f = lowerCase f `elem` map ("setup" <.>) haskellExtensions