import Sq.Parser
import Data.Maybe (fromJust)

import Text.Parsec
import qualified Sq
    
-- [ f | f <- modules ]
                   
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
eval (Bind m (F x body)) cont = Seq [eval body ((x, a):cont) | a <- as]
    where
      Seq as = eval m cont
eval (VarExpr v) cont = fromJust $ lookup v cont
eval (AppExpr Functions x) cont = let (Module m) = eval x cont in
                                  Seq $ map Function (Sq.functions m)
eval Modules _cont = Seq $ map Module Sq.modules
