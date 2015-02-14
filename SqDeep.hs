import Sq.Parser
import Data.Maybe (fromJust)
import Text.Parsec (parse)
import qualified Sq
    
data Value
    = Module Sq.DbModule
    | Function Sq.DbFunction
    | String String
    | Unit
    | Seq [Value]
      deriving Eq

instance Show Value where
    show (Module m) = Sq.mname m
    show (Function f) = Sq.fname f
    show (Seq vs)   = show vs

type Env = [(Var, Value)]

eval :: Query -> Env -> Value
eval (Bind m (F x body)) cont = Seq $ foldr step [] [eval body ((x, a):cont) | a <- as]
    where
      Seq as = eval m cont
      step (Seq xs) acc = xs ++ acc
eval (VarExpr v) cont = readVar v cont
eval (AppExpr Functions (VarExpr v)) cont = let (Module m) = readVar v cont in
                                            Seq $ map Function (Sq.functions m)
eval (AppExpr Name (VarExpr v)) cont = case readVar v cont of
                                         Module m -> String $ Sq.mname m
                                         Function f -> String $ Sq.fname f
eval Modules _cont = Seq $ map Module Sq.modules
eval (Return e) cont = Seq $ [eval e cont]
eval (StringLit s) _cont = String s
eval (Guard p1 rel p2) cont = if evalRel p1' rel p2' then Seq [Unit] else Seq []
    where p1' = eval p1 cont
          p2' = eval p2 cont

evalRel p1 Eq p2 = p1 == p2

readVar :: Var -> Env -> Value
readVar v cont = fromJust $ lookup v cont
