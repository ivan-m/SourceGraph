{-# LANGUAGE FlexibleInstances #-}

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
   Module      : Parsing.ParseModule
   Description : Parse a Haskell module.
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : GPL-3 or later.
   Maintainer  : Ivan.Miljenovic@gmail.com

   Parse a Haskell module.
 -}
module SourceGraph.Parsing.ParseModule(parseModule) where

import SourceGraph.Parsing.State
import SourceGraph.Parsing.Types

import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Syntax

import           Control.Arrow (second, (***))
import           Control.Monad (liftM, liftM2)
import           Data.Char     (isUpper)
import           Data.Foldable (foldrM)
import qualified Data.Map      as M
import           Data.Maybe    (catMaybes, fromJust, fromMaybe, maybeToList)
import           Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import           Data.Set      (Set)
import qualified Data.Set      as S

-- -----------------------------------------------------------------------------

parseModule       :: ParsedModules -> Module l -> ParsedModule
parseModule hms m = pm
    where
      mns = moduleNames hms
      pm = runPState hms mns blankPM $ parseInfo m

-- -----------------------------------------------------------------------------

class ModuleItem a where
    parseInfo :: a -> PState ()

instance (ModuleItem a) => ModuleItem [a] where
    parseInfo = mapM_ parseInfo

-- -----------------------------------------------------------------------------
-- Overall Module

splitModuleHeadMaybe :: Maybe (ModuleHead l) -> (ModuleName l,Maybe [ExportSpec l])
splitModuleHeadMaybe Nothing = (ModuleName (error "noloc") "Main",Nothing)
splitModuleHeadMaybe (Just h) = splitModuleHead h

splitModuleHead :: ModuleHead l -> (ModuleName l,Maybe [ExportSpec l])
splitModuleHead (ModuleHead _ n _ es) = (n,fmap exports es)
    where
    exports (ExportSpecList _ xs) = xs

instance ModuleItem (Module l) where
    parseInfo (Module _ h _ is ds) = do
        let (nm,es) = splitModuleHeadMaybe h
        let mn = createModule' nm
        pm <- get
        put $ pm { moduleName = mn }
        parseInfo es
        parseInfo is
        parseInfo ds

-- -----------------------------------------------------------------------------
-- Imports

instance ModuleItem (ImportDecl l) where
    parseInfo iDcl
        = do mns <- getModuleNames
             ms  <- getModules
             pm  <- get
             let nm  = getModName mns . nameOf $ importModule iDcl
                 md  = M.lookup nm ms
                 es  = imported nm md
                 im  = mi nm es
                 pm' = pm { imports = M.insert nm im (imports pm) }
             put pm'
      where
        mi nm es = I { fromModule   = nm
                     , importQuald  = importQualified iDcl
                     , importedAs   = fmap nameOf $ importAs iDcl
                     , importedEnts = es
                     }

        imported nm Nothing
            = case importSpecs iDcl of
                Just (ImportSpecList _ False is) -> mkSet
                                   $ map (createEnt nm) is
                _               -> S.empty
        imported _  (Just ml)
            = case importSpecs iDcl of
                Nothing         -> exprtd
                Just (ImportSpecList _ False is) -> lstd is
                Just (ImportSpecList _ True is) -> exprtd `S.difference` lstd is
            where
              exprtd = exports ml
              exLk = exportLookup ml
              lstd = mkSet . map (listedEnt ml exLk)

-- | Guesstimate the correct 'Entity' designation for those from
--   external modules.
createEnt                      :: ModName -> ImportSpec l -> [Entity]
createEnt mn (IVar _ n)        = [Ent mn (nameOf n) NormalEntity]
createEnt mn (IThingWith _ n cs) = map (\c -> Ent mn c (eT c)) cs'
    where
      n' = nameOf n
      cs' = map nameOf cs
      isD = isUpper . head
      isDta = any (isUpper . head) cs'
      mkData c | isD c     = Constructor n'
               | otherwise = RecordFunction n' -- Nothing
      mkClass _ = ClassMethod n'
      eT = if isDta then mkData else mkClass
createEnt _  _                 = []

-- | Determine the correct 'Entity' designation for the listed import item.
listedEnt                         :: ParsedModule -> EntityLookup
                                     -> ImportSpec l -> [Entity]
listedEnt _  el (IVar _ n)        = [lookupEntity' el $ nameOf n]
listedEnt _  _  IAbs{}            = []
listedEnt pm _  (IThingAll _ n)     = esFrom dataDecls ++ esFrom classDecls
                                    -- one will be empty
    where
      esFrom f = maybe [] M.elems $ M.lookup (nameOf n) (f pm)
listedEnt pm _  (IThingWith _ n cs) = esFrom dataDecls ++ esFrom classDecls
    where
      el f = M.lookup (nameOf n) $ f pm
      esFrom = maybe [] (\lk -> map (lookupEntity' lk . nameOf) cs) . el

-- -----------------------------------------------------------------------------
-- Exports

-- If the export list is unspecified but there is a function called
-- "main" defined, then it is defined as the export list (otherwise
-- all top-level items are exported).
instance ModuleItem (Maybe [ExportSpec l]) where
    parseInfo Nothing    = do pm <- get
                              fpm <- getFutureParsedModule
                              el <- getLookup
                              let mainFunc = M.lookup (Nothing,"main") el
                                  es = maybe (exportableEnts fpm) S.singleton mainFunc
                              put $ pm { exports = es }
    parseInfo (Just eps) = do pm <- get
                              fpm <- getFutureParsedModule
                              let el = exportableLookup fpm
                                  es = mkSet $ map (listedExp fpm el) eps
                              put $ pm { exports = es }

-- Doesn't work on re-exported Class/Data specs.
listedExp                           :: ParsedModule -> EntityLookup
                                       -> ExportSpec l -> [Entity]
listedExp _  el (EVar _ qn)         = maybe [] (return . lookupEntity el)
                                      $ qName qn
listedExp _  _  EAbs{}              = []
--listedExp pm _  (EThingAll qn)      = esFrom dataDecls ++ esFrom classDecls
--                                      -- one will be empty
--    where
--      esFrom f = fromMaybe []
--                 $ do n <- liftM snd $ qName qn
--                      el <- M.lookup n $ f pm
--                      return $ M.elems el
listedExp pm _  (EThingWith _ _ qn cs)  = esFrom dataDecls ++ esFrom classDecls
    where
      esFrom f = fromMaybe [] $ do mn <- fmap snd $ qName qn
                                   el <- M.lookup mn $ f pm
                                   return $ map (lookupEntity' el . nameOf) cs
listedExp pm _  (EModuleContents _ m) = fromMaybe []
                                      . fmap (S.toList . importedEnts)
                                      . M.lookup (createModule' m)
                                      $ imports pm

-- -----------------------------------------------------------------------------
-- Main part of the module

instRuleHead :: InstRule l -> InstHead l
instRuleHead (IParen _ r) = instRuleHead r
instRuleHead (IRule l _ _ h) = h

splitInstHead :: InstHead l -> (QName l,[Type l])
splitInstHead (IHCon _ n) = (n,[])
splitInstHead (IHInfix _ t n) = (n,[t])
splitInstHead (IHParen _ h) = splitInstHead h
splitInstHead (IHApp _ h t) = (n,ts++[t])
    where (n,ts) = splitInstHead h

declHeadName :: DeclHead l -> Name l
declHeadName (DHead l n) = n
declHeadName (DHInfix l _ n) = n
declHeadName (DHParen _ h) = declHeadName h
declHeadName (DHApp _ h _) = declHeadName h

instance ModuleItem (Decl l) where
    -- Type alias
    parseInfo TypeDecl{} = return ()
    -- Type Families: don't seem to have any entities.
    parseInfo TypeFamDecl{} = return ()
    -- Data or Newtype
    parseInfo (DataDecl _ _ _ h cs _) = do
        let nm = declHeadName h
        let d = nameOf nm
        els <- mapM (addConstructor d . unQConDecl) cs
        pm <- get
        let el = M.unions els
            dds' = M.insert d el $ dataDecls pm
        put $ pm { dataDecls = dds' }
    -- GADT-style Data or Newtype
    parseInfo (GDataDecl _ _ _ h _ gds _) = do
        let n = declHeadName h
        m <- getModuleName
        pm <- get
        let d = nameOf n
            el = addGConstructors m d gds
            dds' = M.insert d el $ dataDecls pm
        put $ pm { dataDecls = dds' }
    -- Data Families: don't seem to have any entities
    parseInfo DataFamDecl{} = return ()
    -- Type families are basically aliases...
    parseInfo TypeInsDecl{} = return ()
    -- Data family instances are pretty much data declarations
    -- Don't add them yet, as can't necessarily go from Type -> Name
    -- todo
    parseInfo DataInsDecl{} = return ()
    -- Same thing as above
    -- todo
    parseInfo GDataInsDecl{} = return ()
    -- Defining a new class
    parseInfo (ClassDecl _ _ h _ cds) = do
        let n = declHeadName h
        let c = nameOf n
        mels <- mapM (addClassDecl c) $ maybe [] id cds
        pm <- get
        let el = M.unions $ catMaybes mels
            cl' = M.insert c el $ classDecls pm
        put $ pm { classDecls = cl' }
    -- Instance of a class
    parseInfo (InstDecl _ _ r ids) = do
        let h = instRuleHead r
        let (n,ts) = splitInstHead h
        let c = snd . fromJust $ qName n
            d = unwords $ map prettyPrint ts
        mapM_ (addInstDecl c d) $ maybe [] id ids
    -- Stand-alone deriving
    parseInfo DerivDecl{} = return ()
    -- Fixity of infix operators
    parseInfo InfixDecl{} = return ()
    -- Default types (Integer, etc.)
    parseInfo DefaultDecl{} = return ()
    -- TH Splicing
    parseInfo SpliceDecl{} = return ()
    -- Type sigs... use the actual function
    parseInfo TypeSig{} = return ()
    -- Actual Function
    parseInfo (FunBind _ ms) = mapM_ addMatch ms
    -- Defining a variable, etc.
    parseInfo pb@PatBind{}
        = do mn <- getModuleName
             el <- getLookup
             pm <- get
             (d,c) <- getDecl pb -- Might as well use this
             -- We can have more than one definition from here, unlike
             -- for Matches.
             let vs = S.map snd d
                 mkE v = Ent mn v NormalEntity
                 es = S.map mkE vs
                 es' = MS.fromList $ S.toList es
                 mkFC e o = FC e (lookupEntity el o) NormalCall
                 mkFCs o = MS.map (flip mkFC o) es'
                 cs = MS.unions . map mkFCs $ MS.toList c
                 pm' = pm { topEnts = topEnts pm `S.union` es
                          , funcCalls = funcCalls pm `MS.union` cs
                          }
             put pm'
    -- The rest are foreign import/export and pragmas
    parseInfo _ = return ()

-- -----------------------------------------------------------------------------
-- Constructors

unQConDecl                        :: QualConDecl l -> ConDecl l
unQConDecl (QualConDecl _ _ _ cd) = cd

addConstructor                 :: DataType -> ConDecl l -> PState EntityLookup
addConstructor d (ConDecl _ n _) = do
    m <- getModuleName
    let n' = nameOf n
        e = Ent m n' (Constructor d)
    return $ M.singleton (Nothing,n') e
addConstructor d (InfixConDecl _ _ n _) = do
    m <- getModuleName
    let n' = nameOf n
        e = Ent m n' (Constructor d)
    return $ M.singleton (Nothing,n') e
addConstructor d (RecDecl _ n fd) = do
    let rbs = map (\(FieldDecl _ x y) -> (x,y)) fd
    m <- getModuleName
    pm <- get
    let n' = nameOf n
        ce = Ent m n' (Constructor d)
        rs = map nameOf $ concatMap fst rbs
        res = map (mkRe m) rs
        es = ce : res
        fcs = MS.fromList $ map (mkFc ce) res
    put $ addFcs pm fcs
    return $ mkEl es
  where
      mkRe m r = Ent m r (RecordFunction d)
      mkFc c r = FC r c RecordConstructor
      addFcs pm fcs = pm { funcCalls = fcs `MS.union` funcCalls pm }

-- -----------------------------------------------------------------------------
-- GADT constructors

addGConstructors     :: ModName -> DataType -> [GadtDecl l] -> EntityLookup
addGConstructors m d = mkEl . map addGConst
    where
      addGConst (GadtDecl _ n _ _) = Ent m (nameOf n) (Constructor d)

-- -----------------------------------------------------------------------------
-- Class declaration

addClassDecl               :: ClassName -> ClassDecl l
                              -> PState (Maybe EntityLookup)
addClassDecl c (ClsDecl _ d) = addCDecl c d
addClassDecl _ _           = return Nothing

addCDecl                    :: ClassName -> Decl l -> PState (Maybe EntityLookup)
addCDecl c (TypeSig _ ns _) = do m <- getModuleName
                                 let ns' = map nameOf ns
                                     eTp = ClassMethod c
                                     es = map (\n -> Ent m n eTp) ns'
                                 return $ Just (mkEl es)
addCDecl c (FunBind _ ms)     = mapM_ (addCMatch c) ms >> return Nothing

addCDecl c pb@PatBind{}     = do mn <- getModuleName
                                 el <- getLookup
                                 pm <- get
                                 (d,cs) <- getDecl pb
                                 let vs = S.map snd d
                                     -- Class-based entities
                                     mkI n = Ent mn n (DefaultInstance c)
                                     mkC n = Ent mn n (ClassMethod c)
                                     cis = S.map (\n -> (mkC n, mkI n)) vs
                                     -- Instance Decls
                                     iDcls = S.map snd cis `S.union` instDecls pm
                                     cis' = MS.fromList $ S.toList cis
                                     -- DefInst calls
                                     mkiCl (f,t) = FC f t DefaultInstDeclaration
                                     ciCls = MS.map mkiCl cis'
                                     -- Calls for that instance
                                     is = MS.map snd cis'
                                     mkFC i o = FC i (lookupEntity el o) NormalCall
                                     mkFCs o = MS.map (flip mkFC o) is
                                     cs' = MS.unions . map mkFCs $ MS.toList cs
                                     pm' = pm { instDecls = iDcls
                                              , funcCalls = funcCalls pm
                                                            `MS.union` ciCls
                                                            `MS.union` cs'
                                              }
                                 put pm'
                                 return Nothing
-- Can't have anything else in classes
addCDecl _ _                = return Nothing

addCMatch     :: ClassName -> Match l -> PState ()
addCMatch c m = do el <- getLookup
                   di <- addFuncCalls (DefaultInstance c) m
                   pm <- get
                   let cfn = name di
                       cf = lookupEntity' el cfn
                       dic = FC cf di DefaultInstDeclaration
                       pm' = pm { instDecls = S.insert di $ instDecls pm
                                , funcCalls = MS.insert dic $ funcCalls pm
                                }
                   put pm'

-- -----------------------------------------------------------------------------
-- Instance Declaration

addInstDecl                    :: ClassName -> DataType -> InstDecl l -> PState ()
addInstDecl c d (InsDecl _ decl) = do
    cs <- addIDecl c d decl
    mn <- getModuleName
    pm <- get
    let fromThisMod = (==) mn . inModule
        cs' = S.filter (not . fromThisMod) cs
        pm' = pm { virtualEnts = virtualEnts pm
            `S.union`
            cs'
            }
    put pm'
addInstDecl _ _ _              = return ()

addIDecl                  :: ClassName -> DataType -> Decl l -> PState (Set Entity)
addIDecl c d (FunBind _ ms) = liftM S.fromList $ mapM (addIMatch c d) ms
addIDecl c d pb@PatBind{} = do mn <- getModuleName
                               el <- getLookup
                               pm <- get
                               pmf <- getFutureParsedModule
                               (df,cs) <- getDecl pb
                               let vs = S.map snd df
                                   -- Class-based entities
                                   mkI n = Ent mn n (ClassInstance c d)
                                   mkC = classFuncLookup c pmf
                                   cis = S.map (\n -> (mkC n, mkI n)) vs
                                   -- Instance Decls
                                   iDcls = S.map snd cis `S.union` instDecls pm
                                   cis' = MS.fromList $ S.toList cis
                                   -- DefInst calls
                                   mkiCl (f,t) = FC f t InstanceDeclaration
                                   ciCls = MS.map mkiCl cis'
                                   -- Calls for that instance
                                   is = MS.map snd cis'
                                   mkFC i o = FC i (lookupEntity el o) NormalCall
                                   mkFCs o = MS.map (flip mkFC o) is
                                   cs' = MS.unions . map mkFCs $ MS.toList cs
                                   pm' = pm { instDecls = iDcls
                                            , funcCalls = funcCalls pm
                                                          `MS.union` ciCls
                                                          `MS.union` cs'
                                            }
                               put pm'
                               return $ S.map fst cis
addIDecl _ _ _            = return S.empty

addIMatch       :: ClassName -> DataType -> Match l -> PState Entity
addIMatch c d m = do pmf <- getFutureParsedModule
                     fi <- addFuncCalls (ClassInstance c d) m
                     pm <- get
                     let cfn = name fi
                         cf = classFuncLookup c pmf cfn
                         ic = FC cf fi InstanceDeclaration
                         pm' = pm { instDecls = S.insert fi $ instDecls pm
                                  , funcCalls = MS.insert ic $ funcCalls pm
                                  }
                     put pm'
                     return cf

-- Must use the future ParsedModule.  Can't use internalLookup, since
-- it includes the virtuals and results in infinite loops as it tries
-- to lookup values that it creates...
--
-- Note: we assume that if a class method is explicitly imported from
-- an external module, then it will be obvious that it is a class
-- method because otherwise the class won't be imported either.
classFuncLookup         :: ClassName -> ParsedModule -> EntityName -> Entity
classFuncLookup c pmf n = case inModule e of
                            UnknownMod -> e { eType = ClassMethod c }
                            _          -> e
    where
      e = lookupEntity' knownMethods n
      knownMethods = localMethods `M.union` importMethods
      localMethods = fromMaybe M.empty $ c `M.lookup` classDecls pmf
      importMethods = M.filter (isMethod . eType) $ allImports pmf
      isMethod (ClassMethod c') = c == c'
      isMethod _                = False

-- -----------------------------------------------------------------------------
-- For top-level functions

addMatch   :: Match l -> PState ()
addMatch m = do e <- addFuncCalls NormalEntity m
                pm <- get
                put $ pm { topEnts = S.insert e $ topEnts pm }

-- -----------------------------------------------------------------------------

-- Add the appropriate 'FunctionCall' values and return the created
-- 'Entity'.  The 'FunctionCall's have @callType = NormalCall@.
addFuncCalls      :: EntityType -> Match l -> PState Entity
addFuncCalls et m = do mn <- getModuleName
                       el <- getLookup
                       pm <- get
                       (d,c) <- getMatch m
                       let nm = snd $ S.findMin d
                           f = Ent { inModule  = mn
                                   , name      = nm -- Assume non-qualified...
                                   , eType     = et
                                   }
                           cs = MS.map (mkFC el f) c
                           pm' = pm { funcCalls = cs `MS.union` funcCalls pm }
                       put pm'
                       return f
    where
      mkFC el l qn = FC l (lookupEntity el qn) NormalCall

-- -----------------------------------------------------------------------------

-- Pulling apart sub-components

-- None of these really need PState... I thought they did mid-write,
-- refactored them all to use it and then after I'd finished realised
-- they didn't.  Too late to change, so they can stay this way.

type Defined = Set QEntityName
type Called = MultiSet QEntityName
type DefCalled = (Defined, Called)

getMatch                         :: Match l -> PState DefCalled
getMatch (Match _ n ps rhs bs) = do
    (avs, afs) <- getPats ps
    rcs <- getRHS rhs
    (bds, bcs) <- getMaybeBindings bs
    let vs = avs `S.union` bds
        fs = MS.unions [afs, rcs, bcs]
        cs = defElsewhere fs vs
    return (S.singleton $ nameOf' n, cs)
getMatch (InfixMatch _ _ n ps rhs bs) = do
    (avs, afs) <- getPats ps
    rcs <- getRHS rhs
    (bds, bcs) <- getMaybeBindings bs
    let vs = avs `S.union` bds
        fs = MS.unions [afs, rcs, bcs]
        cs = defElsewhere fs vs
    return (S.singleton $ nameOf' n, cs)

-- In a pattern, all variables are ones that have just been defined to
-- use in that function, etc.
getPat                     :: Pat l -> PState DefCalled
-- Variable
getPat (PVar _ n)            = return $ onlyVar n
-- Literal value
getPat PLit{}              = return noEnts
-- n + k pattern
getPat (PNPlusK _ n _)       = return $ onlyVar n
-- e.g. a : as
getPat (PInfixApp _ p1 c p2) = do
    (v1, c1) <- getPat p1
    (v2, c2) <- getPat p2
    return ( v1 `S.union` v2, insQName c $ c1 `MS.union` c2)
-- Data constructor + args
getPat (PApp _ qn ps)        = liftM (second $ insQName qn) $ getPats ps
-- Tuple
getPat (PTuple _ _ ps)       = getPats ps
-- Explicit list
getPat (PList _ ps)          = getPats ps
-- Parens around a Pat
getPat (PParen _ p)          = getPat p
-- Record pattern
getPat (PRec _ q ps)         = liftM (second (insQName q) . sMsUnions)
                             $ mapM getPField ps
-- @-pattern
getPat (PAsPat _ n p)        = liftM (sMsMerge (onlyVar n)) $ getPat p
-- _
getPat (PWildCard _)          = return noEnts
-- ~pat
getPat (PIrrPat _ p)         = getPat p
-- pattern with explicit type-sig
getPat (PatTypeSig _ p _)  = getPat p
-- View pattern (function -> constructor) [this avoids an explicit
-- case statement]
getPat (PViewPat _ e p)      = do
    ec <- getExp e
    (pd,pc) <- getPat p
    return (pd, ec `MS.union` pc)
-- HaRP... no idea now to deal with this
getPat PRPat{}             = return noEnts
-- !foo
getPat (PBangPat _ p)        = getPat p
-- The rest are XML and TH patterns
getPat _                   = return noEnts

getPats :: [Pat l] -> PState DefCalled
getPats = liftM sMsUnions . mapM getPat

insQName       :: QName l -> Called -> Called
insQName qn sq = maybe sq (flip MS.insert sq) $ qName qn

onlyVar   :: (Named n) => n -> DefCalled
onlyVar n = (S.singleton $ nameOf' n, MS.empty)

-- Punned fields: not registered as variables
-- Record wildcards: nothing returned
getPField                  :: PatField l -> PState DefCalled
getPField (PFieldPat _ qn p) = liftM (second $ insQName qn) $ getPat p
getPField (PFieldPun _ n)    = return (S.empty, MS.fromList . maybeToList $ qName n)
getPField (PFieldWildcard _)   = return noEnts

-- Still have to take care of function calls here somewhere...
-- Nope: trying to get the overall list of functions called here...
-- and _then_ create function calls to them!

getMaybeBindings              :: Maybe (Binds l) -> PState DefCalled
getMaybeBindings Nothing = return noEnts
getMaybeBindings (Just b) = getBindings b

getBindings              :: Binds l -> PState DefCalled
getBindings (BDecls _ ds)  = liftM sMsUnions $ mapM getDecl ds
getBindings (IPBinds _ is) = liftM sMsUnions $ mapM getIPBinds is

getIPBinds                :: IPBind l -> PState DefCalled
getIPBinds (IPBind _ _ e) = liftM noDefs $ getExp e

getDecl                    :: Decl l -> PState DefCalled
getDecl (FunBind _ ms)       = liftM sMsUnions $ mapM getMatch ms
getDecl (PatBind _ p r bs) = do (pd,pc) <- getPat p
                                rc      <- getRHS r
                                (bd,bc) <- getMaybeBindings bs
                                let fs = MS.unions [pc, rc, bc]
                                    cs = defElsewhere fs bd
                                return (pd, cs)
getDecl _                  = return noEnts


getRHS                   :: Rhs l -> PState Called
getRHS (UnGuardedRhs _ e)  = getExp e
getRHS (GuardedRhss _ grs) = liftM MS.unions $ mapM getGRhs grs

getGRhs                     :: GuardedRhs l -> PState Called
getGRhs (GuardedRhs _ ss e) = do (sf,sc) <- getStmts ss
                                 ec <- getExp e
                                 return $ defElsewhere' sf (sc `MS.union` ec)

-- Gah, this might be wrong...

getExp          :: Exp l -> PState Called
getExp (Var _ qn) = return $ maybeEnt qn
getExp IPVar{}  = return MS.empty
getExp (Con _ qn)  = return $ maybeEnt qn
getExp Lit{}              = return MS.empty
getExp (InfixApp _ e1 o e2) = do
    e1' <- getExp e1
    e2' <- getExp e2
    let o' = maybeEnt o
    return $ e1' `MS.union` e2' `MS.union` o'
getExp (App _ ef vf) = liftM2 MS.union (getExp ef) (getExp vf)
getExp (NegApp _ e)      = getExp e
getExp (Lambda _ ps e) = do (pd,pc) <- getPats ps
                            e' <- getExp e
                            return $ defElsewhere' pd
                                       $ MS.union pc e'
getExp (Let _ bs e) = do
    (bd,bc) <- getBindings bs
    e' <- getExp e
    return $ defElsewhere' bd (MS.union bc e')
getExp (If _ i t e)  = getExps [i,t,e]
getExp (Case _ e as) = do
    e' <- getExp e
    as' <-  mapM getAlt as
    return $ MS.unions (e':as')
getExp (Do _ ss) = chainedCalled $ map getStmt ss
getExp (MDo _ ss) = liftM (uncurry defElsewhere') $ getStmts ss
getExp (Tuple _ _ es) = getExps es
getExp (TupleSection _ _ mes) = getExps $ catMaybes mes
getExp (List _ es) = getExps es
getExp (Paren _ e) = getExp e
getExp (LeftSection _ e o) = liftM (MS.union (maybeEnt o)) $ getExp e
getExp (RightSection _ o e) = liftM (MS.union (maybeEnt o)) $ getExp e
getExp (RecConstr _ qn fus) = liftM (MS.union (maybeEnt qn)) $ getFUpdates fus
getExp (RecUpdate _ e fus)  = liftM2 MS.union (getExp e) (getFUpdates fus)
getExp (EnumFrom _ e) = getExp e
getExp (EnumFromTo _ e1 e2) = liftM2 MS.union (getExp e1) (getExp e2)
getExp (EnumFromThen _ e1 e2) = liftM2 MS.union (getExp e1) (getExp e2)
getExp (EnumFromThenTo _ e1 e2 e3) = liftM2 MS.union (getExp e1)
                                   $ liftM2 MS.union (getExp e2) (getExp e3)
getExp (ListComp _ e qss) = liftM2 MS.union (getExp e) $ getQStmts qss
getExp (ParComp _ e qsss) = liftM2 MS.union (getExp e) . liftM MS.unions
                          $ mapM getQStmts qsss
getExp (ExpTypeSig _ e _) = getExp e
getExp (VarQuote _ qn) = return $ maybeEnt qn
getExp (Proc _ p e) = do
    (pd,pc) <- getPat p
    c <- getExp e
    return $ pc `MS.union` defElsewhere c pd
getExp (RightArrApp _ e1 e2) = liftM2 MS.union (getExp e1) (getExp e2)
getExp (LeftArrApp _ e1 e2) = liftM2 MS.union (getExp e1) (getExp e2)
getExp (RightArrHighApp _ e1 e2) = liftM2 MS.union (getExp e1) (getExp e2)
getExp (LeftArrHighApp _ e1 e2) = liftM2 MS.union (getExp e1) (getExp e2)
-- Everything else is TH, XML or Pragmas
getExp _ = return MS.empty

getExps :: [Exp l] -> PState Called
getExps = liftM MS.unions . mapM getExp

chainedCalled :: [PState DefCalled] -> PState Called
chainedCalled = foldrM go MS.empty
    where
      go s cs = liftM (rmVars cs) s
      rmVars cs (d,c) = defElsewhere cs d `MS.union` c

getQStmt                      :: QualStmt l -> PState DefCalled
getQStmt (QualStmt _ s)         = getStmt s
getQStmt (ThenTrans _ e)        = liftM noDefs $ getExp e
getQStmt (ThenBy _ e1 e2)       = liftM noDefs $ liftM2 MS.union (getExp e1) (getExp e2)
getQStmt (GroupBy _ e)          = liftM noDefs $ getExp e
getQStmt (GroupUsing _ e)       = liftM noDefs $ getExp e
getQStmt (GroupByUsing _ e1 e2) = liftM noDefs
                                $ liftM2 MS.union (getExp e1) (getExp e2)

getQStmts :: [QualStmt l] -> PState Called
getQStmts = chainedCalled . map getQStmt

getFUpdates :: [FieldUpdate l] -> PState Called
getFUpdates = liftM MS.unions . mapM getFUpdate

getFUpdate                    :: FieldUpdate l -> PState Called
getFUpdate (FieldUpdate _ qn e) = liftM (MS.union (maybeEnt qn)) $ getExp e
getFUpdate (FieldPun _ n)       = return . MS.fromList . maybeToList $ qName n
getFUpdate _                  = return MS.empty

getAlt                  :: Alt l -> PState Called
getAlt (Alt _ p gas bs) = do (pd,pc) <- getPat p
                             gc <- getRHS gas
                             (bd,bc) <- getMaybeBindings bs
                             let d = pd `S.union` bd
                                 c = pc `MS.union` gc `MS.union` bc
                             return $ defElsewhere c d

getStmt                   :: Stmt l -> PState DefCalled
getStmt (Generator _ p e) = do (pf,pc) <- getPat p
                               ec <- getExp e
                               return (pf, defElsewhere' pf (MS.union pc ec))
getStmt (Qualifier _ e)     = liftM noDefs $ getExp e
getStmt (LetStmt _ bs)      = getBindings bs
getStmt (RecStmt _ ss)      = getStmts ss

getStmts :: [Stmt l] -> PState DefCalled
getStmts = liftM sMsUnions . mapM getStmt

noDefs :: Called -> DefCalled
noDefs = (,) S.empty

maybeEnt :: (QNamed a) => a -> Called
maybeEnt = maybe MS.empty MS.singleton . qName

noEnts :: DefCalled
noEnts = (S.empty, MS.empty)

-- -----------------------------------------------------------------------------

class Named a where
    nameOf :: a -> String

instance Named (Name l) where
    nameOf (Ident _ n)  = n
    nameOf (Symbol _ n) = n

nameOf' :: (Named n) => n -> QEntityName
nameOf' = (,) Nothing . nameOf

instance Named (CName l) where
    nameOf (VarName _ n) = nameOf n
    nameOf (ConName _ n) = nameOf n

instance Named (ModuleName l) where
    nameOf (ModuleName _ m) = m

-- | Create the 'ModName'.
createModule' :: ModuleName l -> ModName
createModule' = createModule . nameOf

class QNamed a where
    qName :: a -> Maybe QEntityName

instance QNamed (QName l) where
    qName (Qual _ m n) = Just (Just $ nameOf m, nameOf n)
    qName (UnQual _ n) = Just (Nothing, nameOf n)
    qName Special{}  = Nothing

instance QNamed (QOp l) where
    qName (QVarOp _ qn) = qName qn
    qName (QConOp _ qn) = qName qn

sMsUnions :: (Ord a, Ord b) => [(Set a, MultiSet b)] -> (Set a, MultiSet b)
sMsUnions = (S.unions *** MS.unions) . unzip

sMsMerge           :: (Ord a, Ord b) => (Set a, MultiSet b)
                      -> (Set a, MultiSet b) -> (Set a, MultiSet b)
sMsMerge (s1, ms1) = S.union s1 *** MS.union ms1

defElsewhere      :: Called -> Defined -> Called
defElsewhere ms s = MS.fromMap $ fs `M.difference` ifs
    where
      fs = MS.toMap ms
      ifs = M.fromList . map (flip (,) () ) $ S.toList s

defElsewhere' :: Defined -> Called -> Called
defElsewhere' = flip defElsewhere

-- -----------------------------------------------------------------------------
