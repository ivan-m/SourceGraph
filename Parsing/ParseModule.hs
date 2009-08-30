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
parseModule :: HaskellModules -> Module -> HaskellModule
parseModule hm (Module _ md _ _ exps imp decls) = Hs { moduleName = m
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

-- | Create the 'ModName'.
createModule' :: ModuleName -> ModName
createModule' = createModule . modName

-- | Parse all import declarations, and create the 'FunctionLookup' map
--   on the imports.
parseImports       :: HaskellModules -> [ImportDecl]
                   -> ([HsImport],FunctionLookup)
parseImports hm is = (is', flookup)
    where
      is' = catMaybes $ map (parseImport hm) is
      flookup = createLookup $ concatMap importList is'

-- | Convert the import declaration.  Assumes that all functions imported
--   are indeed valid.
parseImport :: HaskellModules -> ImportDecl -> Maybe HsImport
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
      getImport (IVar nm) = Just (nameOf nm)
      getImport _           = Nothing



-- | Parsing the export list.
parseExports   :: ModName -> [ExportSpec] -> [Function]
parseExports m = catMaybes . map parseExport
    where
      -- We only care about exported functions.
      parseExport (EVar qn) = fmap (setFuncModule m) $ hsName qn
      parseExport _           = Nothing

-- | Parse the contents of the module.  For each stand-alone function,
--   return it along with all other known functions that it calls.
functionCalls         :: ModName -> FunctionLookup -> [Decl]
                      -> [(Function, [Function])]
functionCalls m fl ds = catMaybes $ map (functionCall m fl) ds

-- | Parse an individual 'HsDecl'.  We only parse 'HsFunBind' and 'HsPatBind'
--   declarations, as they're the only ones that define stand-alone functions.
functionCall :: ModName -> FunctionLookup -> Decl
             -> Maybe (Function, [Function])
functionCall m fl d@FunBind{} = fmap (flip (,) calls) nm
    where
      nm = listToMaybe . setFuncModules m $ functionNames d
      calls = lookupFunctions fl $ hsNames d
functionCall m fl d@PatBind{} = fmap (flip (,) calls) nm
    where
      nm = listToMaybe . setFuncModules m $ functionNames d
      calls = lookupFunctions fl $ hsNames d
functionCall _ _ _ = Nothing

-- -----------------------------------------------------------------------------

-- Utility functions.

-- | Extract the actual name of the 'Module'.
modName            :: ModuleName -> String
modName (ModuleName m) = m

-- | A true list-difference function.  The default '\\\\' function only deletes
--   the first instance of each value, this deletes /all/ of them.
diff       :: (Eq a) => [a] -> [a] -> [a]
diff xs ys = filter (not . flip elem ys) xs

-- | Specify the qualification that this function was imported with.
addQual     :: String -> Function -> Function
addQual q f = f { qualdBy = Just q }

-- -----------------------------------------------------------------------------

-- | Parsing of names, identifiers, etc.

nameOf              :: Name -> String
nameOf (Ident  i) = i
nameOf (Symbol s) = s

-- The class of parsed items which represent a single element.
class HsItem f where
    hsName :: f -> Maybe Function

instance HsItem Name where
    hsName nm = Just $ defFunc (nameOf nm)

-- Implicit parameters
instance HsItem IPName where
    hsName (IPDup v) = Just $ defFunc v
    hsName (IPLin v) = Just $ defFunc v

-- Qualified variables and constructors
instance HsItem QName where
    hsName (Qual m nm) = fmap (addQual (modName m)) (hsName nm)
    hsName (UnQual nm) = hsName nm
    -- inbuilt special syntax, e.g. [], (,), etc.
    hsName (Special _) = Nothing

-- Infix operators.
instance HsItem QOp where
    hsName (QVarOp qn) = hsName qn
    hsName (QConOp qn) = hsName qn

-- Operators in infix declarations
instance HsItem Op where
    hsName (VarOp nm) = hsName nm
    hsName (ConOp nm) = hsName nm

-- Items in import statements
instance HsItem CName where
    hsName (VarName nm) = hsName nm
    hsName (ConName nm) = hsName nm

-- We don't care about literals
instance HsItem Literal where
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

instance HsItemList Pat where
    hsNames (PVar var)              = hsName' var
    hsNames (PLit _)                = []
    hsNames (PNeg pat)              = hsNames pat
    hsNames (PInfixApp pat1 _ pat2) = (hsNames pat1)
                                        ++ (hsNames pat2)
    hsNames (PApp _ pats)           = hsNames pats
    hsNames (PTuple pats)           = hsNames pats
    hsNames (PList pats)            = hsNames pats
    hsNames (PParen pat)            = hsNames pat
    hsNames (PRec _ pfields)        = hsNames pfields
    hsNames (PAsPat nm pat)         = hsName' nm
                                        ++ hsNames pat
    hsNames PWildCard               = []
    hsNames (PIrrPat pat)           = hsNames pat
    -- Ignore HaRP and Hsx extensions for now
    hsNames _                         = []

instance HsItemList PatField where
    hsNames (PFieldPat _ pat) = hsNames pat

instance HsItemList Binds where
    hsNames (BDecls dcls) = hsNames dcls
    hsNames (IPBinds ibs) = hsNames ibs

instance HsItemList Exp where
    hsNames (Var qn)                = hsName' qn
    hsNames (IPVar ip)              = hsName' ip
    hsNames (Con _)                 = []
    hsNames (Lit _)                 = []
    hsNames (InfixApp e1 q e2)      = hsName' q
                                        ++ hsNames e1 ++ hsNames e2
    hsNames (App e1 e2)             = hsNames e1 ++ hsNames e2
    hsNames (NegApp e)              = hsNames e
    hsNames (Lambda _ ps e)         = (hsNames e) `diff` (hsNames ps)
    hsNames (Let bs e)              = (hsNames bs ++ hsNames e)
                                        `diff` (functionNames bs)
    hsNames (If i t e)              = hsNames [i,t,e]
    hsNames (Case e as)             = hsNames e ++ hsNames as
    hsNames (Do stmts)              = hsNames stmts
    hsNames (MDo stmts)             = hsNames stmts
    hsNames (Tuple es)              = hsNames es
    hsNames (TupleSection _)        = [] -- TODO
    hsNames (List es)               = hsNames es
    hsNames (Paren e)               = hsNames e
    hsNames (LeftSection e qop)     = hsNames e ++ hsName' qop
    hsNames (RightSection qop e)    = hsNames e ++ hsName' qop
    hsNames (RecConstr _ flds)      = hsNames flds
    hsNames (RecUpdate e flds)      = hsNames e ++ hsNames flds
    hsNames (EnumFrom f)            = hsNames f
    hsNames (EnumFromTo f t)        = hsNames [f,t]
    hsNames (EnumFromThen f th)     = hsNames [f,th]
    hsNames (EnumFromThenTo f th t) = hsNames [f,th,t]
{-  Need a FuncitonNames instance for QualStmt
    hsNames (ListComp e stmts)      = let svars = functionNames stmts
                                          e' = hsNames e ++ hsNames stmts
                                      in e' `diff` svars
-}
    hsNames (ParComp _ _)           = [] -- TODO
    hsNames (ExpTypeSig _ e _)      = hsNames e
    -- For now, ignore HaRP, TH and Hsx stuff
    hsNames _                         = []

instance HsItemList FieldUpdate where
    hsNames (FieldUpdate _ e) = hsNames e

instance HsItemList Alt where
    hsNames (Alt _ pats rhs bnds) = (rhs' ++ bnds')
                                      `diff` (lhs ++ bndNames)
        where
          lhs = hsNames pats
          bndNames = functionNames bnds
          bnds' = hsNames bnds
          rhs' = hsNames rhs

instance HsItemList GuardedAlts where
    hsNames (UnGuardedAlt e) = hsNames e
    hsNames (GuardedAlts gas) = hsNames gas

instance HsItemList GuardedAlt where
    hsNames (GuardedAlt _ stmts e) = e' `diff` svars
        where
          svars = functionNames stmts
          e' = hsNames e ++ hsNames stmts

instance HsItemList Decl where
    hsNames (FunBind ms)            = hsNames ms
    hsNames (PatBind _ _ _ rhs bds) = (bds' ++ rhs') `diff` rNames
        where
          rNames = functionNames bds
          bds' = hsNames bds
          rhs' = hsNames rhs
    hsNames _                       = []


instance HsItemList Match where
    hsNames (Match _ _ pats _ rhs bnds) = (rhs' ++ bnds')
                                          `diff` (lhs ++ bndNames)
        where
          lhs = hsNames pats
          bndNames = functionNames bnds
          bnds' = hsNames bnds
          rhs' = hsNames rhs

instance HsItemList Rhs where
    hsNames (UnGuardedRhs e) = hsNames e
    hsNames (GuardedRhss rs) = hsNames rs

instance HsItemList GuardedRhs where
    hsNames (GuardedRhs _ stmts e) = e' `diff` svars
        where
          svars = functionNames stmts
          e' = hsNames e ++ hsNames stmts

instance HsItemList Stmt where
    hsNames (Generator _ _ e) = hsNames e
    hsNames (Qualifier e)     = hsNames e
    hsNames (LetStmt bnds)    = hsNames bnds

instance HsItemList IPBind where
    hsNames (IPBind _ ip e) = e' `diff` ip'
        where
          ip' = hsName' ip
          e' = hsNames e

-- -----------------------------------------------------------------------------

-- | Those parsed elements that represent a function.
class FunctionNames fn where
    functionNames :: fn -> [Function]

instance (FunctionNames fn) => FunctionNames [fn] where
    functionNames = concatMap functionNames

instance FunctionNames Stmt where
    functionNames (Generator _ p _) = hsNames p
    functionNames (Qualifier _)     = []
    functionNames (LetStmt bnds)    = functionNames bnds

instance FunctionNames Match where
    functionNames (Match _ nm _ _ _ _) = hsName' nm

instance FunctionNames Decl where
    functionNames (FunBind ms)        = functionNames ms
    functionNames (PatBind _ p _ _ _) = hsNames p
    functionNames _                   = []

instance FunctionNames Binds where
    functionNames (BDecls decls) = functionNames decls
    functionNames (IPBinds bnds) = functionNames bnds

instance FunctionNames IPBind where
    functionNames (IPBind _ ipn _) = hsName' ipn
