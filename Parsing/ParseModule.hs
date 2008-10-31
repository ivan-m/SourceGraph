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
module Parsing.ParseModule (parseModule) where

import Parsing.Types

import Language.Haskell.Exts.Syntax

import Data.List
import Data.Maybe
import qualified Data.Map as M

-- -----------------------------------------------------------------------------

{- |
   Parses a Haskell Module in 'HsModule' format into the internal
   'HaskellModule' format.  The 'HaskellModules' parameter is used
   to look up the export lists of all imported functions.

   The resulting 'HaskellModule'\'s 'functions' field is a 'Map' that
   maps all functions defined in this module to those functions
   accessible to them from modules in the first parameter.

   At the moment, parsing works only on stand-alone functions, i.e. no
   data structures, class declarations or instance declarations.
 -}
parseModule :: HaskellModules -> HsModule -> HaskellModule
parseModule hm (HsModule _ md exps imp decls) = Hs { moduleName = m
                                                   , imports    = imps'
                                                   , exports    = exps'
                                                   , functions  = funcs
                                                   }
    where
      m = createModule' md
      (imps,fl) = parseImports hm imp
      imps' = map fromModule imps
      -- If there isn't an export list, export everything.
      -- The exception is if there isn't an export list but there is a
      -- /main/ function, in which case only export that.
      exps' | isJust exps = parseExports m $ fromJust exps
            | hasMain     = [mainFunc]
            | otherwise   = defFuncs
      mainFunc = F m (nameOf main_name) Nothing
      hasMain = elem mainFunc defFuncs
      -- We utilise "Tying-the-knot" here to simultaneously update the
      -- lookup map as well as utilise that lookup map.
      funcs = functionCalls m fl' decls
      defFuncs = map fst funcs
      flInternal = createLookup defFuncs
      fl' = M.union fl flInternal

-- | Create the 'ModuleName'.
createModule' :: Module -> ModuleName
createModule' = createModule . modName

-- | Parse all import declarations, and create the 'FunctionLookup' map
--   on the imports.
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
      mn = importModule im
      m = createModule' mn
      -- Determine if this module has been imported with an /as/ prefix.
      -- If not, it has the entire name as its prefix.
      qual = fmap modName (importAs im)
      qual' = fromMaybe (modName mn) qual
      -- Try and get the functions that this module exports.
      mExport = exports $ hm M.! m
      mImport = case (importSpecs im) of
                  -- Everything was imported
                  Nothing -> mExport
                  -- Only specific items were imported.
                  Just (True, ims) -> getFunctions m ims
                  -- These items were hidden.
                  Just (False, hd) -> mExport \\ (getFunctions m hd)
      qImport = map (addQual qual') mImport
      imps = if (importQualified im)
             then qImport
             else mImport ++ qImport
      getFunctions m' = map (setModule m') . getItems
      setModule m' f = F m' f Nothing
      getItems = catMaybes . map getImport
      -- We only care about functions, variables, etc.
      getImport (HsIVar nm) = Just (nameOf nm)
      getImport _           = Nothing



-- | Parsing the export list.
parseExports   :: ModuleName -> [HsExportSpec] -> [Function]
parseExports m = catMaybes . map parseExport
    where
      -- We only care about exported functions.
      parseExport (HsEVar qn) = fmap (setFuncModule m) $ hsName qn
      parseExport _           = Nothing

-- | Parse the contents of the module.  For each stand-alone function,
--   return it along with all other known functions that it calls.
functionCalls         :: ModuleName -> FunctionLookup -> [HsDecl]
                      -> [(Function, [Function])]
functionCalls m fl ds = catMaybes $ map (functionCall m fl) ds

-- | Parse an individual 'HsDecl'.  We only parse 'HsFunBind' and 'HsPatBind'
--   declarations, as they're the only ones that define stand-alone functions.
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

-- -----------------------------------------------------------------------------

-- Utility functions.

-- | Extract the actual name of the 'Module'.
modName            :: Module -> String
modName (Module m) = m

-- | A true list-difference function.  The default '\\\\' function only deletes
--   the first instance of each value, this deletes /all/ of them.
diff       :: (Eq a) => [a] -> [a] -> [a]
diff xs ys = filter (not . flip elem ys) xs

-- | Specify the qualification that this function was imported with.
addQual     :: String -> Function -> Function
addQual q f = f { qualdBy = Just q }

-- -----------------------------------------------------------------------------

-- | Parsing of names, identifiers, etc.

nameOf              :: HsName -> String
nameOf (HsIdent  i) = i
nameOf (HsSymbol s) = s

-- The class of parsed items which represent a single element.
class HsItem f where
    hsName :: f -> Maybe Function

instance HsItem HsName where
    hsName nm = Just $ defFunc (nameOf nm)

-- Implicit parameters
instance HsItem HsIPName where
    hsName (HsIPDup v) = Just $ defFunc v
    hsName (HsIPLin v) = Just $ defFunc v

-- Qualified variables and constructors
instance HsItem HsQName where
    hsName (Qual m nm) = fmap (addQual (modName m)) (hsName nm)
    hsName (UnQual nm) = hsName nm
    -- inbuilt special syntax, e.g. [], (,), etc.
    hsName (Special _) = Nothing

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

-------------------------------------------------------------------------------

-- | Functions, blocks, etc.

class HsItemList vs where
    hsNames :: vs -> [Function]

-- | A \"compatibility\" function to pseudo-convert an instance of 'HsItem'
--   into one that acts like one from 'HsItemList'
hsName'        :: (HsItem i) => i -> [Function]
hsName' i = maybeToList (hsName i)

-- A list of 'HsItemList' instances can be treated as a single instance.
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
    hsNames (HsPAsPat nm pat)         = hsName' nm
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

-- -----------------------------------------------------------------------------

-- | Those parsed elements that represent a function.
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
