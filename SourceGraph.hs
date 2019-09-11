{-
Copyright (C) 2009 Ivan Lazar Miljenovic <Ivan.Miljenovic@gmail.com>

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
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : GPL-3 or later.
   Maintainer  : Ivan.Miljenovic@gmail.com

   The executable file for the /SourceGraph/ programme.

   This was written as part of my mathematics honours thesis,
   /Graph-Theoretic Analysis of the Relationships in Discrete Data/.
 -}
module Main where

import Language.Haskell.SourceGraph.CabalInfo
import Language.Haskell.SourceGraph.Parsing
import Language.Haskell.SourceGraph.Parsing.Types(nameOfModule,ParsedModules,ModName(..))
import Language.Haskell.SourceGraph.Analyse

import Data.Graph.Analysis
import Data.Graph.Analysis.Reporting.Pandoc
import Data.GraphViz.Commands(quitWithoutGraphviz)

import qualified Data.Map as M
import System.IO(hPutStrLn, stderr)
import System.Directory( getCurrentDirectory
                       , doesFileExist)
import System.FilePath( dropFileName
                      , (</>))
import System.Random(newStdGen)
import System.Environment(getArgs)
import Control.Arrow(second)
import Control.Monad(liftM)
--import Control.Exception(SomeException(..), try)

import Data.Version(showVersion)
import qualified Paths_SourceGraph as Paths(version)

-- -----------------------------------------------------------------------------

main :: IO ()
main = do quitWithoutGraphviz noGraphvizErr
          input <- getArgs
          mInfo <- getPkgInfo input
          case mInfo of
            Nothing -> putErrLn "No parseable package information found."
            Just (f,(nm,exps))
                -> do let dir = dropFileName f
                      dir' <- if null dir
                                then getCurrentDirectory
                                else return dir
                      (failed,hms) <- parseFilesFrom dir'
                      mapM_ (putErrLn . ("Could not parse source file "++)) failed
                      analyseCode dir nm exps failed hms
  where
    noGraphvizErr = "ERROR:" ++ programmeName ++ " requires the tools from \
                    \http://graphviz.org to be installed."

programmeName :: String
programmeName = "SourceGraph"

programmeVersion :: String
programmeVersion = showVersion Paths.version

putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr

-- -----------------------------------------------------------------------------

getPkgInfo :: [String] -> IO (Maybe (FilePath, (String, [ModName])))
getPkgInfo [] = do putErrLn "Please provide either a Cabal file \
                            \or a Haskell source file as an argument."
                   return Nothing
getPkgInfo [f]
    | isCabalFile f   = withF parseCabal'
    | isHaskellFile f = withF parseMain
    where
      withF func = do ex <- doesFileExist f
                      if ex
                         then addF $ func f
                         else do putErrLn "The provided file does not exist."
                                 return Nothing
      addF = fmap (fmap ((,) f))
getPkgInfo _        = do putErrLn "Please provide a single Cabal \
                                  \or Haskell source file as an argument."
                         return Nothing

parseCabal' :: FilePath -> IO (Maybe (String, [ModName]))
parseCabal' = liftM (fmap (second (map fpToModule))) . parseCabal

isCabalFile :: FilePath -> Bool
isCabalFile = hasExt "cabal"

parseMain    :: FilePath -> IO (Maybe (String, [ModName]))
parseMain fp = do (_,pms) <- parseHaskellFiles [fp]
                  let mn = fst $ M.findMin pms
                  return $ Just (nameOfModule mn, [mn])



-- -----------------------------------------------------------------------------

analyseCode                    :: FilePath -> String -> [ModName]
                                  -> [FilePath] -> ParsedModules -> IO ()
analyseCode fp nm exps fld hms = do d <- today
                                    g <- newStdGen
                                    let dc = doc d g
                                    docOut <- createDocument pandocHtml' dc
                                    case docOut of
                                      Just path -> success path
                                      Nothing   -> failure
    where
      pandocHtml' = alsoSaveDot pandocHtml
      graphdir = "graphs"
      doc d g = Doc { rootDirectory  = rt
                    , fileFront      = nm
                    , graphDirectory = graphdir
                    , title          = t
                    , author         = a
                    , date           = d
                    , legend         = sgLegend
                    , content        = notes : c g
                    }
      rt = fp </> programmeName
      sv s v = s ++ " (version " ++ v ++ ")"
      t = Grouping [Text "Analysis of", Text nm]
      a = unwords [ "Analysed by", sv programmeName programmeVersion
                  , "using", sv "Graphalyze" version]
      c g = analyse g exps hms
      success fp' = putStrLn $ unwords ["Report generated at:",fp']
      failure = putErrLn "Unable to generate report"
      notes = Section (Text "Notes")
              $ (if null fld then id else (++[failed]))
                [msg, implicitMsg, linkMsg]
      msg = Paragraph [ Text "Please note that the source-code analysis in this\
                             \ document is not necessarily perfect: "
                      , Emphasis $ Text programmeName
                      , Text " is "
                      , Bold $ Text "not"
                      , Text " a refactoring tool, and it's usage of Classes is\
                              \ still premature."
                      ]
      implicitMsg = Paragraph [ Text "Implicitly exported entities refer to\
                                      \ class methods that are instantiated\
                                      \ but defined elsewhere, or"
                              , BlankSpace
                              , DocLink (Text "entities whose names start with\
                                               \ an underscore")
                                        (Web "http://www.haskell.org/ghc/docs/latest/html/users_guide/options-sanity.html")
                              , BlankSpace
                              , Text ".  Note that even for "
                              , Emphasis $ Text "Main"
                              , BlankSpace
                              , Text "modules, these implicit exports are included."
                              ]
      linkMsg = Paragraph [Text "All graph visualisations link to larger \
                                 \SVG versions of the same graph."]
      failed = Section (Text "Parsing Failures")
               [ Paragraph [Text "The following source files were unable\
                                 \ to be parsed; this may result in some\
                                 \ analysis failures:"]
               , Itemized $ map (Paragraph . return . Text) fld
               ]
