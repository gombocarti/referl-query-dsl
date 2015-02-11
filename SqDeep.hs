import Sq.Parser
import Control.Monad.Reader    
import Data.Maybe (fromJust)
import Control.Monad.State

import qualified Sq
    

-- [ f | f <- modules ]
                   
data Value = Module Sq.Name
           | Seq [Value]
             deriving Show

type Context = [(Var, Value)]

eval :: Query -> Context -> Value
eval (Bind Modules (F x body)) cont = Seq [eval body ((x, Module $ Sq.mname m):cont) | m <- Sq.modules]
eval (VarExpr v) cont = fromJust $ lookup v cont
