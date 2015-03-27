{-# LANGUAGE GADTs #-}
module Types where
import Data.List (intercalate)

import qualified Sq (DbModule, DbFunction, Named, ExprType, DbFunctionType)

-- |Identifiers.
type Id = String

data TQuery a where
    TAppExpr :: TQuery (a -> b) -> TQuery a -> TQuery b
    TBind  :: TQuery [a] -> TF (a -> [b]) -> TQuery [b]
    TReturn :: TQuery a -> TQuery [a]
    TUnionExpr :: TQuery [a] -> TQuery [a] -> TQuery [a]
    TVarExpr :: Id -> TQuery a
    TGuard :: TQuery Bool -> TQuery [()]
    TRelation :: Ord a => TQuery a -> Binop -> TQuery a -> TQuery Bool
    TStringLit ::  String -> TQuery String
    TModules :: TQuery [Sq.DbModule]
    TFunctions :: TQuery (Sq.DbModule -> [Sq.DbFunction])
    TName :: Sq.Named a => TQuery (a -> String)
    TArity :: TQuery (Sq.DbFunction -> Int)
    TUnit :: TQuery ()

data TF a where
    TF :: TQuery a -> TQuery b -> TF (a -> b)

-- |Untyped syntax tree type for queries.
data UQuery
    = UQuery UQuery  -- ^ Root of syntax tree
    | UAppExpr Id [UQuery]
    | UFunExpr Id
    | UFunComp [Id]
    | UBind UQuery UF
    | UFunDef Id [Id] UQuery
    | UWith [UQuery] UQuery
    | UReturn UQuery
    | UTuple [UQuery]
    | UGroupBy Id UQuery
    | UVarExpr Id
    | UGuard UQuery
    | URelation Binop UQuery UQuery
--    | UFunRef Id
    | URef Id
    | UDataConst Id
    | UStringLit String
    | UNumLit Int
    | UExprTypeLit Sq.ExprType
    | UFunRecurLit Sq.DbFunctionType
    | UModules
    | UFiles
    | UAtFile
    | UAtModule 
    | UAtFunction
    | UAtExpr
      deriving Eq

data TUQuery = UQuery ::: Typ

{-
-- |Applicable functions of the query language.
data UFun
    = UFunctions -- ^ Functions of a module.
    | UPath      -- ^ Path of a loaded file.
    | UDir       -- ^ Directory containing a file.
    | UFileName  -- ^ Name of a loaded file.
    | UIsModule  -- ^ True if the file contains a module.
    | UFile      -- ^ File of a module.
    | UModule    -- ^ Module of a file.
    | UDefModule -- ^ Module of function.
    | URecords   -- ^ Records defined in a file.
    | UExports   -- ^ Exported functions of a module.
    | UImports   -- ^ Imported functions of a module.
    | ULoc       -- ^ Lines of code.
    | UFunLoc
    | UFileLoc
    | UName      
    | UFunName
    | UModName
    | UArity
    | UCalls
    | UNull      -- ^ Prelude.null.
    | UNot       -- ^ Prelude.not
    | UElem      -- ^ Prelude.elem 
    | UUnion     -- ^ Data.List.union
    | USubset
    | UAnyIn
    | UFName String -- ^ Function identified by its name.
    | UExported
    | URecursivity
    | UReturns
    | UReferences
    | UParameters
    | UOrigin
    | UReach
    | UFields      -- ^ Fields of a record.
    | UTypeOf
    | UFunParamTypeOf
    | URecFieldTypeOf
    | UExprType
    | UExpressions
    | UFunExpressions
    | USubExpressions
    | UChainN
    | UChainInf
    | UClosureN
    | ULfp
    | UIteration
    | UMax
    | UMin
    | UAverage
    | ULength
    | UDistinct
      deriving (Show, Eq)
-}

-- |Untyped function.
data UF = UF Id UQuery
          deriving (Show,Eq)

data Binop
    = Eq
    | NEq
    | Lt
    | Lte
    | Gt
    | Gte
    | Regexp
      deriving Eq

-- |Types of the query language.
data Typ
    = List Typ
    | Tuple [Typ]
    | Chain Typ
    | Grouped Typ Typ
    | File
    | Mod
    | Fun
    | Expr
    | Macro
    | Record
    | RecordField
    | Spec
    | SpecParam
    | FunParam
    | Type    -- ^ Sq.DbType
    | ExprType
    | String
    | Int
    | Bool
    | Unit
    | FunRecursivity
    | FilePath
    | TV Char                -- ^ Type variable.
    | Typ :->: Typ           -- ^ Function type.
    | TypConstraint :=>: Typ -- ^ Type constraint.
      deriving Eq

data TypConstraint 
    = Named Typ
    | Referencable Typ
    | Typeable Typ
    | MultiLine Typ
    | MultiExpression Typ
    | Ord Typ
      deriving (Show,Eq)

infixr 2 :->:
infixr 1 :=>:

-- Show instances.

instance Show Binop where
    show Eq     = "=="
    show NEq    = "/="
    show Lt     = "<"
    show Lte    = "<="
    show Gt     = ">"
    show Gte    = ">="
    show Regexp = "=~"

instance Show Typ where
    show (TV c)   = [c]
    show (List t) = "[" ++ show t ++ "]"
    show (Chain t) = "Chain " ++ show t
    show (Tuple components) = "(" ++ intercalate "," (map show components) ++ ")"
    show (constr :=>: t) = show constr ++ " => " ++ show t
    show (f@(_ :->: _) :->: b) = "(" ++ show f ++ ") -> " ++ show b
    show (a :->: b) = show a ++ " -> " ++ show b
    show File = "File"
    show Mod = "Module"
    show Fun = "Function"
    show Expr = "Expression"
    show Record = "Record"
    show RecordField = "RecordField"
    show FunRecursivity = "FunRecursivity"
    show Type = "Type"
    show FunParam = "FunParam"
    show ExprType = "ExprType"
    show String = "String"
    show Int = "Int"
    show Bool = "Bool"
    show FilePath = "FilePath"

instance Show TUQuery where
    show (q ::: t) = show q ++ " :: " ++ show t

instance Show UQuery where
    show (URef name) = name
    show (UAppExpr f args) = f ++ " " ++ unwords (map show args)
    show (UBind a (UF _ (UReturn _))) = show a ++ " }"
    show (UBind a (UF "()" b)) = show a ++ ", " ++ show b
    show (UBind a (UF f b)) = f ++ " <- " ++ show a ++ ", " ++ show b
    show (UReturn _) = ""
    show (UWith defs q) = "with \n" ++ unlines (map show defs) ++ show q
    show (UFunDef f args body) = f ++ " " ++ unwords args ++ " = " ++ show body
    show (UGuard g) = show g
    show (URelation op a b) = show a ++ " " ++ show op ++ " " ++ show b
    show (UQuery q) = showReturn q ++ show q
    show UModules = "modules"
    show UFiles   = "files"
    show UAtModule = "atModule"
    show UAtFile  = "atFile"
    show UAtExpr = "atExpression"
    show UAtFunction = "atFunction"

showReturn :: UQuery -> String
showReturn (UBind _ (UF _ q)) = showReturn q
showReturn (UReturn ret)      = "{ " ++ show ret ++ " | "
showReturn _ = ""
