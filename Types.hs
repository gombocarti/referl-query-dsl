{-# LANGUAGE GADTs #-}
module Types where
import Data.List (intercalate)
import Text.Parsec.Pos (SourcePos)

import qualified Sq (ExprType, DbFunctionType)

-- |Identifiers.
type Id = String

data TQuery a where
    TAppExpr :: TQuery (a -> b) -> TQuery a -> TQuery b
    TBind  :: TQuery [a] -> TF (a -> [b]) -> TQuery [b]
    TReturn :: TQuery a -> TQuery [a]
    TVarExpr :: Id -> TQuery a
    TGuard :: TQuery Bool -> TQuery [()]
    TStringLit ::  String -> TQuery String
    TNumLit :: Int -> TQuery Int
    TUnit :: TQuery ()

data TF a where
    TF :: TQuery a -> TQuery b -> TF (a -> b)

-- |Untyped syntax tree type for queries.
data UQuery
    = UQuery UQuery  -- ^ Root of syntax tree
    | UCompr UQuery [UQuery]
    | UAppExpr UQuery UQuery
    | UFunExpr Id
    | UFunComp [UQuery]
    | UBind UQuery UF
    | UBind' Id UQuery
    | UReturn UQuery
    | UGuard UQuery UQuery
    | ULambda Id UQuery
    | UFunDef Id [Id] UQuery SourcePos
    | UWith [UQuery] UQuery
    | UTuple [UQuery]
    | UVarExpr Id
--    | UFunRef Id
    | URef Id
    | UDataConst Id
    | UStringLit String
    | UNumLit Int
    | UBoolLit Bool
    | UExprTypeLit Sq.ExprType
    | UFunRecurLit Sq.DbFunctionType
--     deriving (Eq)
      deriving (Eq,Show)
data TUQuery = UQuery ::: Typ

-- |Untyped function.
data UF = UF Id UQuery
          deriving (Show,Eq)
{-
data Binop
    = Eq
    | NEq
    | Lt
    | Lte
    | Gt
    | Gte
    | Regexp
      deriving Eq
-}
-- |Types of the query language.
data Typ
    = Set Typ
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
    | TypVar String          -- ^ Type variable.
    | Typ :->: Typ           -- ^ Function type.
    | TypConstraint :=>: Typ -- ^ Type constraint.
      deriving Eq

data TypConstraint 
    = Named Typ
    | Referencable Typ
    | Typeable Typ
    | MultiLine Typ
    | MultiExpression Typ
    | MultiSet Typ
    | Ord Typ
      deriving (Show,Eq)

infixr 2 :->:
infixr 1 :=>:

-- Show instances.
{-
instance Show Binop where
    show Eq     = "=="
    show NEq    = "/="
    show Lt     = "<"
    show Lte    = "<="
    show Gt     = ">"
    show Gte    = ">="
    show Regexp = "=~"
-}

instance Show Typ where
    show (TypVar v)   = v
    show (Set t) = "{" ++ show t ++ "}"
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
    show Spec = "Spec"
    show Macro = "Macro"
    show SpecParam = "SpecParam"
    show (Grouped a b) = "Grouped " ++ show a ++ " " ++ show b
    show ExprType = "ExprType"
    show String = "String"
    show Int = "Int"
    show Bool = "Bool"
    show Unit = "()"
    show FilePath = "FilePath"

instance Show TUQuery where
    show (q ::: t) = show q ++ " :: " ++ show t

{-
instance Show UQuery where

    showsPrec _ (URef name) = showString name
    showsPrec d (UAppExpr f args) = showParen (d > appPrec) $
                                    showString f .
                                    foldr (.) (showString "") (map showArg args) 
        where appPrec = 10
              showArg arg = showString " " . showsPrec (appPrec + 1) arg
    showsPrec d (UBind a (UF "()" b)) = showsPrec d a .
                                        showString ", " .
                                        showsPrec d b
    showsPrec d (UBind a (UF x (UReturn _))) = showString x .
                                               showString " <- " .
                                               showsPrec d a .
                                               showString " }"
    showsPrec d (UBind a (UF f b)) = showString f .
                                     showString  " <- " .
                                     showsPrec d a .
                                     showString ", " . 
                                     showsPrec d b
    showsPrec _ (UReturn _) = showString ""
    showsPrec d (UWith defs q) = showString "with \n" . 
                                 foldr (.) (showsPrec d q) (map showDef defs)
        where showDef def = showsPrec d def . showString "\n"
    showsPrec d (UFunDef f args body _) = showString f .
                                          showString " " .
                                          showArgs .
                                          showString " = " .
                                          showsPrec d body
        where showArgs = foldr (.) (showString "") (map showString args)
    showsPrec d (UGuard g) = showsPrec d g
    showsPrec d (URelation op a b) = showParen (d > relPrec) $
                                     showsPrec (relPrec + 1) a .
                                     showString " " .
                                     shows op .
                                     showString " " .
                                     showsPrec (relPrec + 1) b
        where relPrec = 4
    showsPrec d (UQuery q) = showReturn q . showsPrec d q
    showsPrec _ UModules = showString "modules"
    showsPrec _ UFiles   = showString "files"
    showsPrec _ UAtModule = showString "atModule"
    showsPrec _ UAtFile  = showString "atFile"
    showsPrec _ UAtExpr = showString "atExpression"
    showsPrec _ UAtFunction = showString "atFunction"
    showsPrec _ (UStringLit s) = shows s
    showsPrec _ (UNumLit n) = shows n
    showsPrec _ (UBoolLit b) = shows b


showReturn :: UQuery -> ShowS
showReturn (UBind _ (UF _ q)) = showReturn q
showReturn (UReturn ret)      = showString "{ " .
                                shows ret .
                                showString " | "
showReturn _ = showString ""

-}
