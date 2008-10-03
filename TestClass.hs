{-
Prototype Haskell -> [(Function, [Function])] parser
Real version should use Maybe's
-}

module Test where

import System.IO
import System.Environment

import Data.List
import Control.Arrow

import Language.Haskell.Parser
import Language.Haskell.Pretty
import Language.Haskell.Syntax

main = do
       input <- getArgs -- get the arguments
       let filename = (head input) :: FilePath
           mode = ParseMode filename
           parser = parseModuleWithMode mode
       -- read the file
       fileContents <- readFile filename
       case (parser fileContents) of
         ParseOk p -> putStrLn (parseResult p)
         _         -> putStrLn "something wrong..."

class HsItem a where
    hsName :: a -> String

instance HsItem HsMatch where
    hsName (HsMatch _ n _ _ _) = hsName n

instance HsItem HsName where
    hsName (HsIdent  i) = i
    hsName (HsSymbol s) = s

class HsItemList a where
    hsNames :: a -> [String]

declarations (HsModule _ _ _ _ decls) = decls

parseResult p = unlines (map (show . hsNames) decls)
    where
      decls = declarations p

matchOther (HsMatch _ _ vars rhs _) = (concatMap hsNames vars, hsNames rhs)

instance HsItemList HsDecl where
    hsNames (HsClassDecl _ ctxt nme nms decls) = hsNames decls
    hsNames (HsInstDecl _ ctxt qnme tps decls) = hsNames decls
    hsNames (HsFunBind mtchs) = nub . concatMap usedFuncs $ mtchs
        where 
          usedFuncs = uncurry (flip (\\)) . matchOther
    hsNames (HsPatBind _ pats rhs decls) = hsNames decls
    hsNames _                            = []

instance (HsItemList a) => HsItemList [a] where
    hsNames = nub . concatMap hsNames

instance HsItemList HsPat where
    hsNames (HsPVar var)               = [hsName var]
    hsNames (HsPLit lit)               = [hsName lit]
    hsNames (HsPNeg pat)               = hsNames pat
    hsNames (HsPInfixApp pat1 qn pat2) = (hsName qn)
                                         : (hsNames pat1)
                                         ++ (hsNames pat2)
    hsNames (HsPApp qn pats)           = concatMap hsNames pats
    hsNames (HsPTuple pats)            = concatMap hsNames pats
    hsNames (HsPList pats)             = concatMap hsNames pats
    hsNames (HsPParen pat)             = hsNames pat
    hsNames (HsPRec qn pfields)        = [hsName qn]
    hsNames (HsPAsPat name pat)        = (hsName name):(hsNames pat)
    hsNames HsPWildCard                = []
    hsNames (HsPIrrPat pat)            = hsNames pat

instance HsItem HsQName where
    hsName (Qual mod name) = hsName name
    hsName (UnQual name)   = hsName name
-- inbuilt special syntax, e.g. [], (,), etc.
    hsName (Special _)     = ""

-- convert the literal to a string
instance HsItem HsLiteral where
    hsName (HsChar ch)        = [ch]
    hsName (HsString str)     = str
    hsName (HsInt int)        = show int
    hsName (HsFrac frac)      = show frac
    hsName (HsCharPrim ch)    = [ch]
    hsName (HsStringPrim str) = str
    hsName (HsIntPrim int)    = show int
    hsName (HsFloatPrim fl)   = show fl
    hsName (HsDoublePrim dbl) = show dbl

instance HsItemList HsRhs where
    hsNames (HsUnGuardedRhs exp) = hsNames exp

instance HsItemList HsExp where
    hsNames (HsVar qname)               = [hsName qname]
    hsNames (HsCon qname)               = [hsName qname]
    hsNames (HsLit lit)                 = [hsName lit]
    hsNames (HsInfixApp e1 op e2)       = (hsNames e1)
                                          ++ (hsNames e2)
    hsNames (HsApp e1 e2)               = (hsNames e1)
                                          ++ (hsNames e2)
    hsNames (HsNegApp exp)              = hsNames exp
    hsNames (HsLambda _ pats exp)       = (hsNames exp) 
                                          \\ (concatMap hsNames pats)
    hsNames (HsLet decls exp)           = (hsNames decls)
                                          ++ (hsNames exp)
    hsNames (HsIf e1 e2 e3)             = (hsNames e1)
                                          ++ (hsNames e2)
                                          ++ (hsNames e3)
    hsNames (HsCase exp alts)           = hsNames exp
    hsNames (HsDo stmts)                = [unlines (map show stmts)]
    hsNames (HsTuple exps)              = concatMap hsNames exps
    hsNames (HsList exps)               = concatMap hsNames exps
    hsNames (HsParen exp)               = hsNames exp
    hsNames (HsLeftSection exp op)      = hsNames exp
    hsNames (HsRightSection op exp)     = hsNames exp
    hsNames (HsRecConstr qn fields)     = [hsName qn]
    hsNames (HsRecUpdate exp fields)    = hsNames exp
    hsNames (HsEnumFrom exp)            = hsNames exp
    hsNames (HsEnumFromTo e1 e2)        = (hsNames e1)
                                          ++ (hsNames e2)
    hsNames (HsEnumFromThen e1 e2)      = (hsNames e1)
                                          ++ (hsNames e2)
    hsNames (HsEnumFromThenTo e1 e2 e3) = (hsNames e1)
                                          ++ (hsNames e2)
                                          ++ (hsNames e3)
    hsNames (HsListComp exp stmts)      = hsNames exp
    hsNames (HsExpTypeSig _ exp qtype)  = hsNames exp
    hsNames (HsAsPat name exp)          = (hsName name)
                                          : hsNames exp
    hsNames HsWildCard                  = ["wild card"]
    hsNames (HsIrrPat exp)              = hsNames exp

instance HsItemList HsStmt where
    hsNames (HsGenerator _ pats exps) = (hsNames exps)
                                        \\ (hsNames pats)
    hsNames (HsQualifier exp)         = hsNames exp
    hsNames (HsLetStmt decls)         = hsNames decls

test x = do y <- putStrLn "2"
            print x
            return ()

testFunc     :: Int -> Int -> Int
testFunc x y = succ x + y * 2
