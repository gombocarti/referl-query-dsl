{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}
module SqDeep where

import Parser (Id, check, UQuery(..), TUQuery(..), UF(..), Binop(..), UFun(..), query)
import Text.Parsec (parse, ParseError)
import qualified Sq
import Control.Monad.Error (throwError)
import Data.List (union)
import Text.Regex.Posix ((=~))
    
type Env = [(Id, Value)]

class Wrap a where
    wrap   :: a -> Value
    unwrap :: Value -> a

instance Wrap Sq.DbModule where
    wrap           = Mod
    unwrap (Mod m) = m

instance Wrap Sq.DbFunction where
    wrap           = Fun
    unwrap (Fun f) = f

instance Wrap Sq.DbExpression where
    wrap            = Expr
    unwrap (Expr e) = e

instance Wrap Sq.DbType where
    wrap            = Type
    unwrap (Type t) = t

instance Wrap Int where
    wrap           = Int
    unwrap (Int n) = n

instance Wrap String where
    wrap              = String
    unwrap (String s) = s

instance Wrap a => Wrap [a] where
    wrap            = Seq . map wrap
    unwrap (Seq xs) = map unwrap xs

data Value
    = File Sq.DbFile
    | Mod Sq.DbModule
    | Fun Sq.DbFunction
    | Expr Sq.DbExpression
    | Type Sq.DbType
    | FunParam Sq.DbFunctionParam
    | Rec Sq.DbRecord
    | RecField Sq.DbRecordField
    | ExprType Sq.ExprType
    | String String
    | Int Int
    | Bool Bool
    | Unit
    | Path FilePath
    | Seq [Value]
      deriving (Eq,Show)

{-
instance Show Value where
    show (File f) = Sq.fpath f
    show (Mod m) = Sq.name m
    show (Fun f) = Sq.name f
    show (Expr e) = Sq.body e
    show (Seq xs) = show xs
    show (Int i) = show i
    show (String s) = s
    show (Bool b) = show b
    show Unit = "()"
-}

instance Ord Value where
    (Int a) <= (Int b) = a <= b
    (String s1) <= (String s2) = s1 <= s2
    (Bool a) <= (Bool b) = a <= b

instance Sq.Named Value where
    name (Mod m) = Sq.name m
    name (Fun f) = Sq.name f
               
run :: String -> Either String Value
run s = either (throwError . show) 
        (\q -> do { q' ::: _ <- check q []; return $ eval q' [] })
        (parse query "" s)

eval :: UQuery -> Env -> Value
eval (UBind m (UF x body)) env = Seq . foldr step [] $ xs
    where
      Seq as = eval m env
      xs = [eval body ((x,a):env) | a <- as]
      step (Seq xs) acc = xs ++ acc
eval (UVarExpr v) env = readVar v env
eval (UAppExpr f args) env = evalApp f args env
eval UModules _env = Seq . map Mod $ Sq.modules
eval UFiles _env = Seq . map File $ Sq.files
eval UAtFunction _env = Fun Sq.atFunction
eval UAtFile _env = File Sq.atFile
eval UAtModule _env = Mod Sq.atModule
eval UAtExpr _env = Expr Sq.atExpression
eval (UReturn e) env = Seq $ [eval e env]
eval (UStringLit s) _env = String s
eval (UNumLit i) _env = Int i
eval (URelation rel p1 p2) env = Bool $ evalRel p1' rel p2'
    where p1' = eval p1 env
          p2' = eval p2 env
eval (UGuard pred) env = if p then Seq [Unit] else Seq []
    where Bool p = eval pred env
eval (UUnionExpr q1 q2) env = Seq $ union v1 v2
    where
      Seq v1 = eval q1 env
      Seq v2 = eval q2 env

evalRel :: Value -> Binop -> Value -> Bool
evalRel a Eq  b = a == b
evalRel a NEq b = a /= b
evalRel a Gt  b = a >  b
evalRel a Gte b = a >= b
evalRel a Lt  b = a <  b
evalRel a Lte b = a <= b
evalRel a Regexp b = s =~ regex
    where String s     = a
          String regex = b

evalApp :: UFun -> [UQuery] -> Env -> Value
evalApp UName [arg] env = String . Sq.name $ eval arg env
evalApp UArity [arg] env = Int . Sq.arity $ f
    where Fun f = eval arg env
evalApp UNot [arg] env = Bool $ not pred
    where Bool pred = eval arg env
evalApp UNull [arg] env = Bool $ null xs
    where Seq xs = eval arg env
evalApp UElem [a,b] env = Bool $ a' `elem` b'
    where a'     = eval a env
          Seq b' = eval b env
evalApp UAllIn [a,b] env = Bool $ a' `elem` b'
    where a'     = eval a env
          Seq b' = eval b env
evalApp UAnyIn [a,b] env = Bool $ as `Sq.any_in` bs
    where Seq as = eval a env
          Seq bs = eval b env
evalApp UCalls [arg] env = Seq . map Fun $ Sq.fcalls f
    where Fun f = eval arg env
evalApp UFunctions [arg] env = Seq . map Fun $ Sq.functions m
    where Mod m = eval arg env
evalApp UExported [arg] env = Bool . Sq.fexported $ f
    where Fun f = eval arg env
evalApp UPath [arg] env = Path . Sq.fpath $ f
    where File f = eval arg env
evalApp UDir [arg] env = Path . Sq.dir $ f
    where File f = eval arg env
evalApp UFileName [arg] env = Path . Sq.filename $ f
    where File f = eval arg env
evalApp UTypeOf [arg] env = case eval arg env of 
                              FunParam p -> Type . Sq.fptype $ p
                              RecField f -> Type . Sq.fieldType $ f
                              Expr e -> ExprType . Sq.etype $ e
evalApp UReturns [arg] env = Seq . map Type . Sq.returns $ f
    where Fun f = eval arg env
evalApp UOrigin [arg] env = Seq . map Expr . Sq.origin . unwrap $ expr
    where expr = eval arg env

readVar :: Id -> Env -> Value
readVar v env = case lookup v env of
                  Just x -> x
                  Nothing -> error $ "undefined variable: " ++ v
