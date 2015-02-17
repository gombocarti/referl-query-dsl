{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances #-}
import Parser
import Data.Maybe (fromJust)
import Text.Parsec (parse)
import qualified Sq

{-    
data Value
    = Module Sq.DbModule
    | Function Sq.DbFunction
    | String String
    | Int Int
    | Bool Bool
    | Unit
    | Seq [Value]
      deriving Eq

-}

type Value = 
    Either Sq.DbModule 
    (Either Sq.DbFunction
    ())

class SubType a b where
    inj :: a -> b
    prj :: b -> Maybe a

instance SubType a (Either a b) where
    inj a = Left a
    prj (Left a) = Just a
    prj (Right _) = Nothing

instance SubType a b => SubType a (Either c b) where
    inj a = Right . inj $ a
    prj (Right b) = prj b
    prj (Left _) = Nothing

{-instance Show Value where
    show (Module m) = Sq.mname m
    show (Function f) = Sq.fname f
    show (Seq vs)   = show vs

instance Sq.Named Value where
    name (Module m) = Sq.mname m
    name (Function f) = Sq.fname f
-}
type Env = [(Var, Value)]

eval :: Query a -> Env -> a
eval (Bind m (F (VarExpr x) body)) cont = concat [eval body ((x, inj a):cont) | a <- as]
    where
      as = eval m cont
{-
eval (VarExpr v) cont = readVar v cont
eval (AppExpr Functions (VarExpr v)) cont = let (Module m) = readVar v cont in
                                            Seq $ map Function (Sq.functions m)
eval (AppExpr Name (VarExpr v)) cont = String . Sq.name $ readVar v cont
eval Modules _cont = Seq $ map Module Sq.modules
eval (Return e) cont = Seq $ [eval e cont]
eval (StringLit s) _cont = String s
eval (Relation p1 rel p2) cont = Bool $ evalRel p1' rel p2'
    where p1' = eval p1 cont
          p2' = eval p2 cont
eval (Guard rel) cont = if p then Seq [Unit] else Seq []
    where Bool p = eval rel cont

evalRel p1 Eq p2 = p1 == p2
-- evalRel p1 Gt p2 = p1 <  p2

readVar :: Var -> Env -> Value
readVar v cont = fromJust $ lookup v cont
-}
