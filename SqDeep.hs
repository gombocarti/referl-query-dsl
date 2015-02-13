import Sq.Parser
import Data.Maybe (fromJust)

import Text.Parsec (parse)
import qualified Sq
    
data Value
    = Module Sq.DbModule
    | Function Sq.DbFunction
    | Seq [Value]

instance Show Value where
    show (Module m) = Sq.mname m
    show (Function f) = Sq.fname f
    show (Seq vs)   = show vs

type Context = [(Var, Value)]

eval :: Query -> Context -> Value
eval (Bind m (F x body)) cont = Seq $ foldr step [] [eval body ((x, a):cont) | a <- as]
    where
      Seq as = eval m cont
      step (Seq xs) acc = xs ++ acc
eval (VarExpr v) cont = readVar v cont
eval (AppExpr Functions (VarExpr v)) cont = let (Module m) = readVar v cont in
                                            Seq $ map Function (Sq.functions m)
eval Modules _cont = Seq $ map Module Sq.modules
eval (Return e) cont = Seq $ [eval e cont]

readVar :: Var -> Context -> Value
readVar v cont = fromJust $ lookup v cont
