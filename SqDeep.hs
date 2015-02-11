{-# LANGUAGE FlexibleContexts #-}

import Text.Parsec
import Text.Parsec.String
import Foreign.Erlang
import Control.Monad.Reader    
import Data.Maybe (fromJust)
import Control.Monad.State
import Control.Applicative ((<*))

import qualified Sq
    
type Var = String
type Fun = String
type Arg = String


data Query 
    = FunExpr Fun
    | AppExpr Fun Arg
    | Bind Query F
    | UnionExpr Query Query
    | VarExpr Var
    | RelExpr Query Binop Query
    | Cont Query Query
    | Sep Query Query
    | Modules
      deriving Show

data F = F Var Query
       deriving Show

data Binop
    = Eq
    | Lt
    | Lte
    | Gt
    | Gte
      deriving Show

-- eval (FunExpr f) = ...

-- [ f | f <- modules ]
                   
lbr :: Parser Char
lbr = char '{'

rbr :: Parser Char
rbr = char '}'

query :: Parser Query
query = between lbr rbr bind

returnExpr :: Parser Query
returnExpr = try app <|> var

{-
body :: Parser Query
body = do r <- returnExpr
          spaces
          _ <- char '|'
          spaces
          qs <- bind `sepBy1` (char ',' <* spaces)
          let cont = foldr1 Cont qs
          return (r `Sep` cont)

-}

body :: Parser Query
body = do r <- returnExpr
          spaces
          _ <- char '|'
          spaces
          b <- bind
          return b

var :: Parser Query
var = do v <- identifier
         return (VarExpr v)

{-
fun :: Parser Query
fun = do v <- identifier
         return (FunExpr v)
-}
app :: Parser Query
app = do f <- identifier
         a <- identifier
         return (AppExpr f a)

{-
bind :: Parser Query
bind = try relation
       <|>
       do v <- identifier
          _ <- string "<-"
          spaces
          x <- boundable
          spaces
          return (Bind x (F [v] b))

-}

-- query = { var <- query | query }

data Value = Module Sq.Name
           | Seq [Value]
             deriving Show

type Context = [(Var, Value)]

eval :: Query -> Context -> Value
eval (Bind Modules (F x body)) cont = Seq [eval body ((x, Module $ Sq.mname m):cont) | m <- Sq.modules]
eval (VarExpr v) cont = fromJust $ lookup v cont

bind :: Parser Query
bind =  do 
  v <- identifier
  _ <- string "<-"
  spaces
  x <- boundable
  spaces
  _ <- string "|"
  spaces
  rest <- boundable
  return (Bind x (F v rest))


boundable :: Parser Query
boundable = try modules <|> var <|> bind  <|> query

modules :: Parser Query
modules = string "modules" `parseAs` Modules

relation :: Parser Query
relation = do a1 <- app
              spaces
              rel <- relop
              a2 <- app
              spaces
              return (RelExpr a1 rel a2)

relop :: Parser Binop
relop = (eq <|> lt <|> gt) <* spaces

eq :: Parser Binop
eq = string "==" `parseAs`  Eq

lt :: Parser Binop
lt = string "<" `parseAs` Lt

gt :: Parser Binop
gt = string ">" `parseAs` Gt

parseAs :: Parser a -> b -> Parser b
parseAs p x = do { _ <- p; return x }

identifier :: Parser String
identifier = many1 lower <* spaces

{-
parseQuery :: String -> Either Query ParseError
parseQuery s = parse 
-}

{-
type Value = Either String ErlType

type Interp a = [a]

type Env = [(Var, Value)]

getEnv :: MonadState Env m => m Env
getEnv = get

readVar :: Var -> Env -> Value
readVar v env = fromJust $ lookup v env

eval :: (Monad m, MonadState Env m) => Query -> m Value
eval (VarExpr v) = do env <- getEnv
                      return (readVar v env)
-}
