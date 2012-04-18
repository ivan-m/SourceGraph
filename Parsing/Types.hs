{-# LANGUAGE TypeFamilies #-}

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
   Module      : Parsing.Types
   Description : Types for parsing Haskell code.
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : GPL-3 or later.
   Maintainer  : Ivan.Miljenovic@gmail.com

   Types for parsing Haskell modules.
 -}
module Parsing.Types where

import Data.Graph.Analysis.Types( ClusterLabel(..)
                                , Rel)
import Data.Graph.Analysis.Reporting(unDotPath)
import Data.GraphViz(GraphID(..), ToGraphID(..), LNodeCluster, NodeCluster(..))
import Data.Graph.Inductive.Graph(LNode)

import Data.Char(isLetter, isDigit)
import Data.List(intercalate)
import Data.Maybe(fromMaybe, isJust)
import qualified Data.Map as M
import Data.Map(Map)
import qualified Data.Set as S
import Data.Set(Set)
import qualified Data.MultiSet as MS
import Data.MultiSet(MultiSet)
import qualified Data.Foldable as F

-- -----------------------------------------------------------------------------

data ParsedModule = PM { moduleName  :: ModName
                       , imports     :: ImpLookup
                       , exports     :: Set Entity
                       , dataDecls   :: DataDecs
                       , classDecls  :: ClassDecs
                         -- These aren't real defined functions
                       , instDecls   :: Set Entity
                       , topEnts     :: Set Entity
                       , virtualEnts :: Set Entity -- used for
                                                   -- drawing
                                                   -- this module
                       , funcCalls   :: MultiSet FunctionCall -- We want each
                                                              -- function
                                                              -- call made,
                                                              -- including duplicates.
                       }
                    deriving (Eq, Ord, Show, Read)

blankPM :: ParsedModule
blankPM = PM { moduleName  = UnknownMod
             , imports     = M.empty
             , exports     = S.empty
             , dataDecls   = M.empty
             , classDecls  = M.empty
             , instDecls   = S.empty
             , topEnts     = S.empty
             , virtualEnts = S.empty
             , funcCalls   = MS.empty
             }

-- | Creates an 'EntityLookup' for the purposes of determining which
--   'Entity's are exported from this module.
exportLookup :: ParsedModule -> EntityLookup
exportLookup = mkLookup' . exports

-- -----------------------------------------------------------------------------

type EntityLookup = Map QEntityName Entity

mkEl :: [Entity] -> EntityLookup
mkEl = M.fromList . map (\e -> ((Nothing, name e), e))

-- | The 'EntityLookup' for use within a module; combines imports with
--   what is defined in the module.
internalLookup    :: ParsedModule -> EntityLookup
internalLookup pm = M.unions [imported, internal, virtuals]
    where
      imported = allImports pm
      internal = exportableLookup pm
      virtuals = mkLookup' $ virtualEnts pm

-- | The defined stand-alone 'Entity's from this module.
exportableLookup    :: ParsedModule -> EntityLookup
exportableLookup pm = M.unions [ decLookup
                               , clLookup
                               , defLookup
                               ]
    where
      decLookup = getLookups dataDecls pm
      clLookup = getLookups classDecls pm
      defLookup = mkLookup' $ topEnts pm

-- | Create an 'EntityLookup' from a particular part of a 'ParsedModule'.
getLookups   :: (ParsedModule -> Map String EntityLookup)
                -> ParsedModule -> EntityLookup
getLookups f = M.unions . M.elems . f

-- | Create an 'EntityLookup' using the given qualification for the
--   'Set' of 'Entity's.
mkLookup    :: EntQual -> Set Entity -> EntityLookup
mkLookup al = mergeMaps . S.map (\e -> M.singleton (al, name e) e)

mkLookup' :: Set Entity -> EntityLookup
mkLookup' = mkLookup Nothing

-- | Find the corresponding 'Entity' for the given name and qualification.
lookupEntity       :: EntityLookup -> QEntityName -> Entity
lookupEntity el qn = fromMaybe unkn $ M.lookup qn el
    where
      unkn = Ent UnknownMod (snd qn) NormalEntity

-- | Find the corresponding 'Entity' for the given name with no qualification.
lookupEntity'    :: EntityLookup -> EntityName -> Entity
lookupEntity' el = lookupEntity el . (,) Nothing

-- -----------------------------------------------------------------------------


-- | A lookup-map of 'HaskellModule's.
type ParsedModules = Map ModName ParsedModule

type ModuleNames = Map String ModName

createModuleMap :: [ParsedModule] -> ParsedModules
createModuleMap = M.fromList
                  . map (\m -> (moduleName m, m))

{-
removeMod :: ParsedModules -> ModName -> ParsedModules
removeMod = flip M.delete
-}
modulesIn :: ParsedModules -> Set ModName
modulesIn = S.fromList . M.keys

moduleNames :: ParsedModules -> ModuleNames
moduleNames = M.fromList . map (\m -> (nameOfModule m, m))
              . M.keys

getModName        :: ModuleNames -> String -> ModName
getModName mns nm = fromMaybe ext $ M.lookup nm mns
    where
      ext = ExtMod nm

moduleRelationships :: Set ParsedModule -> Set (ModName, ModName)
moduleRelationships = setUnion . S.map mkEdges
    where
      mkEdges pm = S.fromList
                   . map ((,) (moduleName pm) . fromModule)
                   . M.elems
                   $ imports pm
{-
hModulesIn :: ParsedModules -> Set ParsedModule
hModulesIn = S.fromList . M.elems
-}

{-
getModule      :: ParsedModules -> ModName -> Maybe ParsedModule
getModule hm m = M.lookup m hm
-}

-- -----------------------------------------------------------------------------

-- | The name of a module.
data ModName = LocalMod { modName :: String }
             | ExtMod   { modName :: String }
             | UnknownMod
               deriving (Eq, Ord, Show, Read)

internalEntity   :: Entity -> Bool
internalEntity e = case inModule e of
                     LocalMod{} -> True
                     _          -> False

type Depth = Int

clusteredModule :: LNode ModName -> NodeCluster (Depth, String) (LNode String)
clusteredModule (n,m) = go 0 $ modulePathOf m
    where
      go _ [m']   = N (n,m')
      go d (p:ps) = C (d,p) $ go (succ d) ps
      go _ []     = error "Shouldn't be able to have an empty module name."

instance ToGraphID ModName where
    toGraphID = toGraphID . unDotPath . nameOfModule

instance ClusterLabel ModName where
    type Cluster ModName = String
    type NodeLabel ModName = String

    cluster = containingDir'
    nodeLabel = nameOfModule'

nameOfModule            :: ModName -> String
nameOfModule UnknownMod = "Unknown Module"
nameOfModule mn         = modName mn

nameOfModule' :: ModName -> String
nameOfModule' = last . modulePathOf

containingDir :: ModName -> String
containingDir = intercalate [moduleSep] . dirPath

containingDir'   :: ModName -> String
containingDir' m = case containingDir m of
                     ""  -> "Project Root"
                     dir -> dir

dirPath :: ModName -> [String]
dirPath = init . modulePathOf

modulePathOf :: ModName -> [String]
modulePathOf = splitSects . nameOfModule
    where
      splitSects mp = case break ((==) moduleSep) mp of
                        (p,"")      -> [p]
                        (p,dmp) -> p : splitSects (tail dmp)


-- | The seperator between components of a module.
moduleSep :: Char
moduleSep = '.'

-- | Create the 'ModName' from its 'String' representation.
createModule :: String -> ModName
createModule = LocalMod

-- -----------------------------------------------------------------------------

type ImpLookup = Map ModName ModImport

-- | The import list of a module.
data ModImport = I { fromModule :: ModName
                   , importQuald :: Bool
                   -- | Any alias used to import it.  Should be Just
                   --   foo if importQuald is True.
                   , importedAs   :: Maybe String
                   -- | The functions from this module that were imported.
                   , importedEnts :: Set Entity
                   }
                 deriving (Eq, Ord, Show, Read)

importsLookup :: [ModImport] -> EntityLookup
importsLookup = M.unions . map importLookup

allImports :: ParsedModule -> EntityLookup
allImports = importsLookup . M.elems . imports

importLookup :: ModImport -> EntityLookup
importLookup hi
    | importQuald hi = with
    | isJust alias   = M.union with without -- alias, no qual
    | otherwise      = without
    where
      es = importedEnts hi
      alias = importedAs hi
      with = mkLookup alias es
      without = mkLookup Nothing es

-- -----------------------------------------------------------------------------

data Entity = Ent { inModule  :: ModName
                  , name      :: EntityName
                  , eType     :: EntityType
                  }
              deriving (Eq, Ord, Show, Read)

instance ClusterLabel Entity where
    type Cluster Entity = ModName
    type NodeLabel Entity = Entity

    cluster = inModule
    nodeLabel = id

clusterEntityM    :: LNode Entity -> LNodeCluster String Entity
clusterEntityM ln = modPathClust id ln (N ln)

modPathClust        :: (String -> a) -> LNode Entity
                       -> LNodeCluster a Entity -> LNodeCluster a Entity
modPathClust f ln b = foldr ($) b $ map (C . f) p
    where
      p = modulePathOf . inModule $ snd ln

clusterEntityM'    :: LNode Entity -> LNodeCluster EntClustType Entity
clusterEntityM' ln = modPathClust ModPath ln (clusterEntity ln)

clusterEntity          :: LNode Entity -> LNodeCluster EntClustType Entity
clusterEntity ln@(_,e) = setClust $ N ln
    where
      setClust = case eType e of
                   (Constructor d)     -> C $ DataDefn d
                   (RecordFunction d)  -> C $ DataDefn d
                   (ClassMethod c)     -> C $ ClassDefn c
                   (DefaultInstance c) -> C (ClassDefn c) . C (DefInst c)
                   (ClassInstance c d) -> C (ClassDefn c) . C (ClassInst c d)
                   _                   -> id

data EntClustType = ClassDefn ClassName
                  | DataDefn DataType
                  | ClassInst ClassName DataType
                  | DefInst ClassName
                  | ModPath String
                    deriving (Eq, Ord, Show, Read)

ctypeID                 :: EntClustType -> GraphID
ctypeID (ClassDefn c)   = toGraphID $ "Class_" ++ escID c
ctypeID (DataDefn d)    = toGraphID $ "Data_" ++ escID d
ctypeID (ClassInst c d) = toGraphID $ "Class_" ++ escID c ++ "_Data_" ++ escID d
ctypeID (DefInst c)     = toGraphID $ "DefaultInstance_" ++ escID c
ctypeID (ModPath p)     = toGraphID $ "Directory_" ++ escID p

escID :: String -> String
escID = filter (\c -> isLetter c || isDigit c || c == '_')

type EntityName = String

type EntQual = Maybe String

type QEntityName = (EntQual, EntityName)

-- | All 'Entity's defined in this 'ParsedModule'.
definedEnts    :: ParsedModule -> Set Entity
definedEnts pm = exportableEnts pm `S.union` instDecls pm

exportableEnts    :: ParsedModule -> Set Entity
exportableEnts pm = S.unions [ decEnts
                             , clEnts
                             , defEnts
                             ]
    where
      decEnts = getEnts dataDecls pm
      clEnts = getEnts classDecls pm
      defEnts = topEnts pm

-- | All 'Entity's used internally within this 'ParsedModule'.
internalEnts    :: ParsedModule -> Set Entity
internalEnts pm = S.union (definedEnts pm) (virtualEnts pm)

getEnts   :: (ParsedModule -> Map String EntityLookup)
             -> ParsedModule -> Set Entity
getEnts f = mkSet
            . map M.elems
            . M.elems
            . f


fullName   :: Entity -> EntityName
fullName e = nameOfModule (inModule e) ++ moduleSep : name e

defEntity    :: EntityName -> Entity
defEntity nm = Ent { inModule  = UnknownMod
                   , name      = nm
                   , eType     = NormalEntity
                   }

setEntModule     :: ModName -> Entity -> Entity
setEntModule m e = e { inModule = m }

setEntModules :: ModName -> Set Entity -> Set Entity
setEntModules = S.map . setEntModule

-- -----------------------------------------------------------------------------

-- Extra type defns.

type DataType = String

type ClassName = String

type DataDecs = Map DataType EntityLookup

type ClassDecs = Map ClassName EntityLookup

data FunctionCall = FC { fromEntity :: Entity
                       , toEntity   :: Entity
                       , callType   :: CallType
                       }
                    deriving (Eq, Ord, Show, Read)

callToRel                 :: FunctionCall -> Rel Entity CallType
callToRel (FC from to tp) = (from, to, tp)

data CallType = NormalCall
              | InstanceDeclaration -- ClassName DataType EntityName
              | DefaultInstDeclaration -- ClassName EntityName
              | RecordConstructor
                deriving (Eq, Ord, Show, Read)

isNormalCall            :: CallType -> Bool
isNormalCall NormalCall = True
isNormalCall _          = False

data EntityType = Constructor DataType
                | RecordFunction DataType -- (Maybe EntityName)
                                          -- same record function in
                                          -- multiple constructors
                | ClassMethod ClassName
                | DefaultInstance ClassName
                | ClassInstance ClassName DataType
                  -- The following three are only for when collapsing.
                | CollapsedData DataType
                | CollapsedClass ClassName
                | CollapsedInstance ClassName DataType
                | NormalEntity -- A function or variable
                  deriving (Eq, Ord, Show, Read)

isData                  :: EntityType -> Bool
isData Constructor{}    = True
isData RecordFunction{} = True
isData _                = False

getDataType                    :: EntityType -> DataType
getDataType (Constructor d)    = d
getDataType (RecordFunction d) = d
getDataType _                  = error "Should not see this"

isClass                   :: EntityType -> Bool
isClass ClassMethod{}     = True
isClass DefaultInstance{} = True
isClass _                 = False

getClassName                     :: EntityType -> ClassName
getClassName (ClassMethod c)     = c
getClassName (DefaultInstance c) = c
getClassName _                   = error "Should not see this"

isInstance                 :: EntityType -> Bool
isInstance ClassInstance{} = True
isInstance _               = False

getInstance                     :: EntityType -> (ClassName, DataType)
getInstance (ClassInstance c d) = (c,d)
getInstance _                   = error "Should not see this"

-- -----------------------------------------------------------------------------

setUnion :: (Ord a) => Set (Set a) -> Set a
setUnion = F.foldl' S.union S.empty

mkSet :: (Ord a) => [[a]] -> Set a
mkSet = S.fromList . concat -- or should this be S.unions . map
                            -- S.fromList ?

mergeMaps :: (Ord k) => Set (Map k a) -> Map k a
mergeMaps = F.foldl' M.union M.empty

