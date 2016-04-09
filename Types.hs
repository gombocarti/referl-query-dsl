module Types
    (Id
    ,Query(..)
    ,QueryT(..)
    ,LF(..)
    ,Typ(..)
    ,TypConstraint(..)
    ,FunctionType(..)
    ,ExprType(..))
where
import Data.List (intercalate)
import Text.Parsec.Pos (SourcePos)

-- |Identifiers.
type Id = String

-- |Untyped syntax tree type for queries.
data Query
    = Query Query  -- ^ Root of syntax tree
--    | Compr Query [Query]
    | AppE Query Query
--    | FunExpr Id
    | FunCompE [Query]
    | BindE Query LF
--    | Bind' Id Query
    | ReturnE Query
    | GuardE Query
--    | Lambda Id Query
    | FunDefE Id [Id] Query SourcePos
    | WithE [Query] Query
    | TupleE [Query]
--    | VarE Id
--    | FunRef Id
    | RefE Id
    | DataConstE Id
    | StringLitE String
    | NumLitE Int
    | BoolLitE Bool
    | ExprTypeLitE ExprType
    | FunRecurLitE FunctionType
--     deriving (Eq)
      deriving (Eq,Show)
data QueryT = Query ::: Typ

-- |Untyped function.
data LF = Lambda Id Query
          deriving (Show,Eq)

-- todo: külön (Types).RefactorErl modulba?
data ExprType
    = Application
    | Arglist
    | Implicit_fun
    | Fun_expr
    | Tuple
    | Atom
    | List
    | Integer
    | Char
    | Float
    | String
    | Variable
    | Cons
    | Field_list
    | Record_expr
    | Record_update
    | Record_access
    | Record_field
    | Match_expr
    | Infix_expr
    | Case
    | If_expr
    | Send_expr
    | Receive_expr  
    | Try_expr      -- ^ try ... end
    | Catch_expr
    | Block_expr    -- ^ begin ... end
    | Parenthesis
      deriving (Show,Eq,Read)

data FunctionType
    = NonRecursive 
    | NonTailRecursive
    | TailRecursive
      deriving (Show,Eq,Read)

-- |Types of the query language.
data Typ
    = Set Typ
    | TupleT [Typ]
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
    | ExprTypeT
    | StringT
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
instance Show Typ where
    show (TypVar v)   = v
    show (Set t) = "{" ++ show t ++ "}"
    show (Chain t) = "Chain " ++ show t
    show (TupleT components) = "(" ++ intercalate "," (map show components) ++ ")"
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
    show ExprTypeT = "ExprType"
    show StringT = "String"
    show Int = "Int"
    show Bool = "Bool"
    show Unit = "()"
    show FilePath = "FilePath"

instance Show QueryT where
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
