{-# LANGUAGE GADTs #-}
module Types where

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
    = UAppExpr UFun [UQuery]
    | UFunExpr UFun
    | UFunComp [UFun]
    | UBind UQuery UF
    | UReturn UQuery
    | UGroupBy UFun UQuery
    | UVarExpr Id
    | UGuard UQuery
    | URelation Binop UQuery UQuery
    | UFunRef UFun
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
      deriving Show

data TUQuery = UQuery ::: Typ deriving Show

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

-- |Untyped function.
data UF = UF Id UQuery
          deriving Show

data Binop
    = Eq
    | NEq
    | Lt
    | Lte
    | Gt
    | Gte
    | Regexp
      deriving (Show,Eq)

-- |Types of the query language.
data Typ
    = List Typ
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
    | A     -- ^ Type variable.
    | B     -- ^ Type variable.
    | Typ :->: Typ  -- ^ Function type.
    | TypConstraint :=>: Typ -- ^ Type constraint.
      deriving (Show,Eq)

data TypConstraint 
    = Named Typ
    | Referencable Typ
    | Typeable Typ
    | MultiLine Typ
    | MultiExpression Typ
    | Ord Typ
      deriving (Show,Eq)

infixr 4 :->:
infixr 3 :=>:
