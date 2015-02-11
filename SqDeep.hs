import Sq.Parser
import Data.Maybe (fromJust)

import Text.Parsec
import qualified Sq
    
-- [ f | f <- modules ]
                   
data Value
    = Module Sq.DbModule
    | Seq [Value]

instance Show Value where
    show (Module m) = Sq.mname m
    show (Seq vs)   = show vs

type Context = [(Var, Value)]

eval :: Query -> Context -> Value
eval (Bind Modules (F x body)) cont = Seq [eval body ((x, Module m):cont) | m <- Sq.modules]
eval (VarExpr v) cont = fromJust $ lookup v cont
