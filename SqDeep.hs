{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}
module SqDeep where

import Parser (Id, check, UQuery(..), TUQuery(..), UF(..), Binop(..), UFun(..), query, runchk)
import Text.Parsec (parse, ParseError)
import qualified Sq
import Control.Monad.Error (throwError)
import Data.List (union)
import Text.Regex.Posix ((=~))
    
type Env = [(Id, Value)]

class Wrap a where
    wrap   :: a -> Value
    unwrap :: Value -> a

instance Wrap Sq.DbFile where
    wrap            = File
    unwrap (File f) = f

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

instance Wrap Sq.ExprType where
    wrap            = ExprType
    unwrap (ExprType t) = t

instance Wrap Int where
    wrap           = Int
    unwrap (Int n) = n

instance Wrap Bool where
    wrap           = Bool
    unwrap (Bool n) = n

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
eval (UBind m (UF x body)) env = concatValue xs
    where
      Seq as = eval m env
      xs = [eval body ((x,a):env) | a <- as]
eval (UVarExpr v) env = readVar v env
eval (UAppExpr f args) env = evalApp f (map (flip eval env) args)
eval UModules _env = wrap Sq.modules
eval UFiles _env = wrap Sq.files
eval UAtFunction _env = wrap Sq.atFunction
eval UAtFile _env = wrap Sq.atFile
eval UAtModule _env = wrap Sq.atModule
eval UAtExpr _env = wrap Sq.atExpression
eval (UReturn e) env = Seq [eval e env]
eval (UStringLit s) _env = wrap s
eval (UNumLit i) _env = wrap i
eval (URelation rel p1 p2) env = wrap $ evalRel p1' rel p2'
    where p1' = eval p1 env
          p2' = eval p2 env
eval (UGuard pred) env = if p then Seq [Unit] else Seq []
    where Bool p = eval pred env
-- works if all function returns [a] for some a:
eval (UFunComp args v) env = foldr step (Seq [eval v env]) args
    where
      step f (Seq val) = concatValue $ map (\arg -> evalApp f [arg]) val

concatValue :: [Value] -> Value
concatValue vals = Seq $ foldr step [] vals
    where step (Seq xs) acc = xs ++ acc

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

evalApp :: UFun -> [Value] -> Value
evalApp UName [arg] = wrap . Sq.name $ arg
evalApp UArity [Fun f] = wrap . Sq.arity $ f
evalApp UNot [Bool pred] = wrap $ not pred
evalApp UNull [Seq xs] = wrap $ null xs
evalApp UElem [a,Seq bs] = wrap $ a `elem` bs
evalApp USubset [Seq as,Seq bs] = wrap $ as `Sq.all_in` bs
evalApp UAnyIn [Seq as,Seq bs] = wrap $ as `Sq.any_in` bs
evalApp UUnion [Seq as,Seq bs] = Seq $ as `union` bs
evalApp UCalls [Fun f] = wrap $  Sq.fcalls f
evalApp UFunctions [Mod m] = wrap $ Sq.functions m
evalApp UExported [Fun f] = wrap . Sq.fexported $ f
evalApp UPath [File f] = wrap . Sq.fpath $ f
evalApp UDir [File f] = wrap . Sq.dir $ f
evalApp UFileName [File f] = wrap . Sq.filename $ f
evalApp UTypeOf [arg] = case arg of 
                          FunParam p -> wrap . Sq.fptype $ p
                          RecField f -> wrap . Sq.fieldType $ f
                          Expr e -> wrap . Sq.etype $ e
evalApp UReturns [Fun f] = wrap . Sq.returns $ f
evalApp UOrigin [Expr expr] = wrap . Sq.origin $ expr
evalApp UReferences [arg] = case arg of
                              Fun f -> wrap . Sq.freferences $ f
                              Rec r -> wrap . Sq.rreferences $ r

readVar :: Id -> Env -> Value
readVar v env = case lookup v env of
                  Just x -> x
                  Nothing -> error $ "undefined variable: " ++ v
