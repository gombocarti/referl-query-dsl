module SqDeep where

import Parser (Id, check, UQuery(..), TUQuery(..), UF(..), Binop(..), UFun(..), query)
import Text.Parsec (parse, ParseError)
import qualified Sq
import Control.Monad.Error (throwError)
import Data.List (union)
    
type Env = [(Id, Value)]

class Wrap a where
    wrap   :: a -> Value
    unwrap :: Value -> a

data Value
    = File Sq.DbFile
    | Mod Sq.DbModule
    | Fun Sq.DbFunction
    | Expr Sq.DbExpression
    | String String
    | Int Int
    | Bool Bool
    | Unit
    | Seq [Value]
      deriving Eq

instance Show Value where
    show (File f) = Sq.fpath f
    show (Mod m) = Sq.name m
    show (Fun f) = Sq.name f
    show (Seq xs) = show xs
    show (Int i) = show i
    show (String s) = s
    show (Bool b) = show b
    show Unit = "()"

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
evalRel p1 Eq  p2 = p1 == p2
evalRel p1 NEq p2 = p1 /= p2
evalRel p1 Gt  p2 = p1 >  p2
evalRel p1 Gte p2 = p1 >= p2
evalRel p1 Lt  p2 = p1 <  p2
evalRel p1 Lte p2 = p1 <= p2

evalApp :: UFun -> [UQuery] -> Env -> Value
evalApp UName [arg] env = String . Sq.name $ eval arg env
evalApp UArity [arg] env = Int . Sq.arity $ f
    where Fun f = eval arg env
evalApp UNot [arg] env = Bool $ not pred
    where Bool pred = eval arg env
evalApp UNull [arg] env = Bool $ null xs
    where Seq xs = eval arg env
evalApp UCalls [arg] env = Seq . map Fun $ Sq.fcalls f
    where Fun f = eval arg env
evalApp UFunctions [UVarExpr v] env = Seq . map Fun $ Sq.functions m
    where Mod m = readVar v env


readVar :: Id -> Env -> Value
readVar v env = case lookup v env of
                  Just x -> x
                  Nothing -> error $ "undefined variable: " ++ v
