{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances, ExistentialQuantification #-}
import Parser (Id, check, UQuery(..), UF(..), Binop(..), UFun(..), query)
import Text.Parsec (parse, ParseError)
import qualified Sq
import Control.Monad.Error (throwError)
import Data.List (union)
    
type Env = [(Id, Value)]

class Wrap a where
    wrap :: a -> Value
    unwrap :: Value -> a

data Value
    = Mod Sq.DbModule
    | Fun Sq.DbFunction
    | String String
    | Int Int
    | Bool Bool
    | Unit
    | Seq [Value]
      deriving Eq

instance Show Value where
    show (Mod m) = Sq.name m
    show (Fun f) = Sq.name f
    show (Seq xs) = show xs
    show (Int i) = show i
    show (String s) = s
    show (Bool b) = show b

instance Ord Value where
    (Int a) <= (Int b) = a <= b
    (String s1) <= (String s2) = s1 <= s2
    (Bool a) <= (Bool b) = a <= b

instance Sq.Named Value where
    name (Mod m) = Sq.name m
    name (Fun f) = Sq.name f
               
run :: String -> Either String Value
run s = either (throwError . show) 
        (\q -> do { check q []; return $ eval q [] })
        (parse query "" s)

eval :: UQuery -> Env -> Value
eval (UBind m (UF x body)) env = Seq . foldr step [] $ xs
    where
      Seq as = eval m env
      step (Seq xs) acc = xs ++ acc
      xs = [eval body ((x,a):env) | a <- as]
eval (UVarExpr v) env = readVar v env
eval (UAppExpr UFunctions (UVarExpr v)) env = Seq . map Fun $ Sq.functions m
    where
      Mod m = readVar v env
eval (UAppExpr UName (UVarExpr v)) env = String . Sq.name $ readVar v env
eval UModules _env = Seq . map Mod $ Sq.modules
eval (UReturn e) env = Seq $ [eval e env]
eval (UStringLit s) _env = String s
eval (UNumLit i) _env = Int i
eval (URelation rel p1 p2) env = Bool $ evalRel p1' rel p2'
    where p1' = eval p1 env
          p2' = eval p2 env
eval (UGuard rel) env = if p then Seq [Unit] else Seq []
    where Bool p = eval rel env
eval (UUnionExpr q1 q2) env = Seq $ union v1 v2
    where
      Seq v1 = eval q1 env
      Seq v2 = eval q2 env

evalRel :: Value -> Binop -> Value -> Bool
evalRel p1 Eq  p2 = p1 == p2
evalRel p1 Gt  p2 = p1 >  p2
evalRel p1 Gte p2 = p1 >= p2
evalRel p1 Lt  p2 = p1 <  p2
evalRel p1 Lte p2 = p1 <= p2

readVar :: Id -> Env -> Value
readVar v env = case lookup v env of
                  Just x -> x
                  Nothing -> error $ "undefined variable: " ++ v
