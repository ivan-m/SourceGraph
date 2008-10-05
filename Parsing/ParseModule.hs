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
   Module      : Parsing.ParseModule
   Description : Parse a Haskell module.
   Copyright   : (c) Ivan Lazar Miljenovic 2008
   License     : GPL-3 or later.
   Maintainer  : Ivan.Miljenovic@gmail.com

   Parse a Haskell module.
 -}
module Parsing.ParseModule where

import Parsing.Types

import Language.Haskell.Exts.Parser hiding (parseModule)
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Syntax

import Data.List
import Data.Maybe
import qualified Data.Map as M
import Data.Map(Map)
import Control.Arrow
import System.FilePath(addExtension, splitExtension)

import Debug.Trace

traceList    :: (Show a) => [a] -> [a]
traceList as = trace (unlines . map show $ as) as

traceMap :: (Show k, Show a) => Map k a -> Map k a
traceMap m = trace (unlines . map show $ M.assocs m) m

tracer   :: (Show a) => a -> a
tracer a = trace (show a) a

tracer' s a = trace (unwords [s,show a]) a

tester f = do let parser = parseModuleWithMode (ParseMode f)
              fileContents <- readFile f
              case (parser fileContents) of
                ParseOk p -> return (Just p)
                _         -> return Nothing

printList :: (Show a) => [a] -> IO ()
printList = mapM_ (putStrLn . show)

parseModule :: HaskellModules -> HsModule -> HaskellModule
parseModule hm (HsModule _ mod exp imp decls) = Hs { moduleName = m
                                                   , imports    = imps'
                                                   , exports    = exps
                                                   , functions  = fs
                                                   }
    where
      m = createModule mod
      (imps,fl) = parseImports hm imp
      imps' = map fromModule imps
      funcs = functionCalls m fl' decls
      defFuncs = map fst funcs
      flInternal = createLookup defFuncs
      fl' = M.union fl flInternal
      fs = M.fromList funcs
      exps = maybe defFuncs (parseExports m) exp

parseExport               :: ModuleName -> HsExportSpec -> Maybe Function
parseExport m (HsEVar qn) = fmap (setFuncModule m) $ hsName qn
parseExport _ _           = Nothing

parseExports   :: ModuleName -> [HsExportSpec] -> [Function]
parseExports m = catMaybes . map (parseExport m)


modName            :: Module -> String
modName (Module m) = m

createModule   :: Module -> ModuleName
createModule m = case (splitExtension $ modName m) of
                   (m',"") -> M Nothing m'
                   (d,m')  -> M (Just d) m'

parseImports       :: HaskellModules -> [HsImportDecl]
                   -> ([HsImport],FunctionLookup)
parseImports hm is = (is', flookup)
    where
      is' = catMaybes $ map (parseImport hm) is
      flookup = createLookup $ concatMap importList is'

-- | Convert the import declaration.  Assumes that all functions imported
--   are indeed valid.
parseImport :: HaskellModules -> HsImportDecl -> Maybe HsImport
parseImport hm im
    | not (M.member m hm) = Nothing -- This module isn't available.
    | otherwise           = Just $ I m qual imps
    where
      m = createModule $ importModule im
      qual = fmap modName $ importAs im
      mExport = exports $ hm M.! m
      mImport = case (importSpecs im) of
                  Nothing -> mExport
                  Just (True, ims) -> getFunctions m ims
                  Just (False, hd) -> mExport \\ (getFunctions m hd)
      qImport = maybe [] (\q -> map (addQual q) mImport) qual
      imps = if (importQualified im)
             then qImport
             else mImport ++ qImport

functionCall :: ModuleName -> FunctionLookup -> HsDecl
             -> Maybe (Function, [Function])
functionCall m fl d@(HsFunBind {}) = fmap (flip (,) calls) nm
    where
      nm = listToMaybe . setFuncModules m $ functionNames d
      calls = lookupFunctions fl $ hsNames d
functionCall m fl d@(HsPatBind {}) = fmap (flip (,) calls) nm
    where
      nm = listToMaybe . setFuncModules m $ functionNames d
      calls = lookupFunctions fl $ hsNames d
functionCall _ _ _ = Nothing

functionCalls         :: ModuleName -> FunctionLookup -> [HsDecl]
                      -> [(Function, [Function])]
functionCalls m fl ds = catMaybes $ map (functionCall m fl) ds

-- \f xs -> let rs = map (f rs) xs in rs

-- -----------------------------------------------------------------------------

diff       :: (Eq a) => [a] -> [a] -> [a]
diff xs ys = filter (not . flip elem ys) xs

getFunctions   :: ModuleName -> [HsImportSpec] -> [Function]
getFunctions m = map (setModule m) . getItems

setModule     :: ModuleName -> String -> Function
setModule m f = F m f Nothing

addQual     :: String -> Function -> Function
addQual q f = f { qualdBy = Just q }


getItems :: [HsImportSpec] -> [String]
getItems = catMaybes . map getImport

-- | We only care about functions, variables, etc.
getImport :: HsImportSpec -> Maybe String
getImport (HsIVar nm) = Just (nameOf nm)
getImport _           = Nothing

-- -----------------------------------------------------------------------------
{-
-- Names, identities, etc.
-}

class HsItem f where
    hsName :: f -> Maybe Function

nameOf              :: HsName -> String
nameOf (HsIdent  i) = i
nameOf (HsSymbol s) = s

-- Implicit parameters
instance HsItem HsIPName where
    hsName (HsIPDup v) = Just $ defFunc v
    hsName (HsIPLin v) = Just $ defFunc v

instance HsItem HsName where
    hsName nm = Just $ defFunc (nameOf nm)

-- Qualified variables and constructors
instance HsItem HsQName where
    hsName (Qual mod name) = fmap (addQual (modName mod)) (hsName name)
    hsName (UnQual name)   = hsName name
    -- inbuilt special syntax, e.g. [], (,), etc.
    hsName (Special _)     = Nothing

-- Infix operators.
instance HsItem HsQOp where
    hsName (HsQVarOp qn) = hsName qn
    hsName (HsQConOp qn) = hsName qn

-- Operators in infix declarations
instance HsItem HsOp where
    hsName (HsVarOp nm) = hsName nm
    hsName (HsConOp nm) = hsName nm

-- Items in import statements
instance HsItem HsCName where
    hsName (HsVarName nm) = hsName nm
    hsName (HsConName nm) = hsName nm

-- We don't care about literals
instance HsItem HsLiteral where
    hsName _ = Nothing

hsName'        :: (HsItem i) => i -> [Function]
hsName' i = maybeToList (hsName i)

class HsItemList vs where
    hsNames :: vs -> [Function]

instance (HsItemList vs) => HsItemList [vs] where
    hsNames vss = concatMap hsNames vss

instance HsItemList HsPat where
    hsNames (HsPVar var)              = hsName' var
    hsNames (HsPLit _)                = []
    hsNames (HsPNeg pat)              = hsNames pat
    hsNames (HsPInfixApp pat1 _ pat2) = (hsNames pat1)
                                        ++ (hsNames pat2)
    hsNames (HsPApp _ pats)           = hsNames pats
    hsNames (HsPTuple pats)           = hsNames pats
    hsNames (HsPList pats)            = hsNames pats
    hsNames (HsPParen pat)            = hsNames pat
    hsNames (HsPRec _ pfields)        = hsNames pfields
    hsNames (HsPAsPat name pat)       = hsName' name
                                        ++ hsNames pat
    hsNames HsPWildCard               = []
    hsNames (HsPIrrPat pat)           = hsNames pat
    -- Ignore HaRP and Hsx extensions for now
    hsNames _                         = []

instance HsItemList HsPatField where
    hsNames (HsPFieldPat _ pat) = hsNames pat

instance HsItemList HsBinds where
    hsNames (HsBDecls dcls) = hsNames dcls
    hsNames (HsIPBinds ibs) = hsNames ibs

instance HsItemList HsExp where
    hsNames (HsVar qn)                = hsName' qn
    hsNames (HsIPVar ip)              = hsName' ip
    hsNames (HsCon _)                 = []
    hsNames (HsLit _)                 = []
    hsNames (HsInfixApp e1 q e2)      = hsName' q
                                        ++ hsNames e1 ++ hsNames e2
    hsNames (HsApp e1 e2)             = hsNames e1 ++ hsNames e2
    hsNames (HsNegApp e)              = hsNames e
    hsNames (HsLambda _ ps e)         = (hsNames e) `diff` (hsNames ps)
    hsNames (HsLet bs e)              = (hsNames bs ++ hsNames e)
                                        `diff` (functionNames bs)
    hsNames (HsDLet ips e)            = (hsNames ips ++ hsNames e)
                                        `diff` (functionNames ips)
    hsNames (HsWith e ips)            = (hsNames ips ++ hsNames e)
                                        `diff` (functionNames ips)
    hsNames (HsIf i t e)              = hsNames [i,t,e]
    hsNames (HsCase e as)             = hsNames e ++ hsNames as
    hsNames (HsDo stmts)              = hsNames stmts
    hsNames (HsMDo stmts)             = hsNames stmts
    hsNames (HsTuple es)              = hsNames es
    hsNames (HsList es)               = hsNames es
    hsNames (HsParen e)               = hsNames e
    hsNames (HsLeftSection e qop)     = hsNames e ++ hsName' qop
    hsNames (HsRightSection qop e)    = hsNames e ++ hsName' qop
    hsNames (HsRecConstr _ flds)      = hsNames flds
    hsNames (HsRecUpdate e flds)      = hsNames e ++ hsNames flds
    hsNames (HsEnumFrom f)            = hsNames f
    hsNames (HsEnumFromTo f t)        = hsNames [f,t]
    hsNames (HsEnumFromThen f th)     = hsNames [f,th]
    hsNames (HsEnumFromThenTo f th t) = hsNames [f,th,t]
    hsNames (HsListComp e stmts)      = let svars = functionNames stmts
                                            e' = hsNames e ++ hsNames stmts
                                        in e' `diff` svars
    hsNames (HsExpTypeSig _ e _)      = hsNames e
    hsNames (HsAsPat _ _)             = [] -- something for FunctionNames?
    hsNames (HsIrrPat _)              = []
    -- For now, ignore HaRP, TH and Hsx stuff
    hsNames _                         = []

instance HsItemList HsFieldUpdate where
    hsNames (HsFieldUpdate _ e) = hsNames e

instance HsItemList HsAlt where
    hsNames (HsAlt _ pats rhs bnds) = (rhs' ++ bnds')
                                      `diff` (lhs ++ bndNames)
        where
          lhs = hsNames pats
          bndNames = functionNames bnds
          bnds' = hsNames bnds
          rhs' = hsNames rhs

instance HsItemList HsGuardedAlts where
    hsNames (HsUnGuardedAlt e) = hsNames e
    hsNames (HsGuardedAlts gas) = hsNames gas

instance HsItemList HsGuardedAlt where
    hsNames (HsGuardedAlt _ stmts e) = e' `diff` svars
        where
          svars = functionNames stmts
          e' = hsNames e ++ hsNames stmts

instance HsItemList HsDecl where
    hsNames (HsFunBind ms)          = hsNames ms
    hsNames (HsPatBind _ _ rhs bds) = (bds' ++ rhs') `diff` rNames
        where
          rNames = functionNames bds
          bds' = hsNames bds
          rhs' = hsNames rhs
    hsNames _                       = []


instance HsItemList HsMatch where
    hsNames (HsMatch _ _ pats rhs bnds) = (rhs' ++ bnds')
                                          `diff` (lhs ++ bndNames)
        where
          lhs = hsNames pats
          bndNames = functionNames bnds
          bnds' = hsNames bnds
          rhs' = hsNames rhs

instance HsItemList HsRhs where
    hsNames (HsUnGuardedRhs e) = hsNames e
    hsNames (HsGuardedRhss rs) = hsNames rs

instance HsItemList HsGuardedRhs where
    hsNames (HsGuardedRhs _ stmts e) = e' `diff` svars
        where
          svars = functionNames stmts
          e' = hsNames e ++ hsNames stmts

instance HsItemList HsStmt where
    hsNames (HsGenerator _ _ e) = hsNames e
    hsNames (HsQualifier e)     = hsNames e
    hsNames (HsLetStmt bnds)    = hsNames bnds

instance HsItemList HsIPBind where
    hsNames (HsIPBind _ ip e) = e' `diff` ip'
        where
          ip' = hsName' ip
          e' = hsNames e

class FunctionNames fn where
    functionNames :: fn -> [Function]

instance (FunctionNames fn) => FunctionNames [fn] where
    functionNames = concatMap functionNames

instance FunctionNames HsStmt where
    functionNames (HsGenerator _ p _) = hsNames p
    functionNames (HsQualifier _)     = []
    functionNames (HsLetStmt bnds)    = functionNames bnds

instance FunctionNames HsMatch where
    functionNames (HsMatch _ nm _ _ _) = hsName' nm

instance FunctionNames HsDecl where
    functionNames (HsFunBind ms)      = functionNames ms
    functionNames (HsPatBind _ p _ _) = hsNames p
    functionNames _                   = []

instance FunctionNames HsBinds where
    functionNames (HsBDecls decls) = functionNames decls
    functionNames (HsIPBinds bnds) = functionNames bnds

instance FunctionNames HsIPBind where
    functionNames (HsIPBind _ ipn _) = hsName' ipn
