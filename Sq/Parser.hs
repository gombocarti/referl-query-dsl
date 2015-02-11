module Sq.Parser where

import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<*))

type Var = String
type Fun = String
type Arg = String

data Query 
    = AppExpr Query Query
    | Bind Query F
    | UnionExpr Query Query
    | VarExpr Var
    | RelExpr Query Binop Query
    | Modules
    | Functions
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

lbr :: Parser Char
lbr = char '{'

rbr :: Parser Char
rbr = char '}'

query :: Parser Query
query = between lbr rbr bind

returnExpr :: Parser Query
returnExpr = try app <|> var

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

app :: Parser Query
app = do f <- functions
         spaces
         a <- identifier
         spaces
         return (AppExpr f (VarExpr a))

-- query = { var <- query | query }

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
boundable = try modules <|> try app <|> var <|> bind  <|> query

modules :: Parser Query
modules = string "modules" `parseAs` Modules

functions :: Parser Query
functions = string "functions" `parseAs` Functions

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
