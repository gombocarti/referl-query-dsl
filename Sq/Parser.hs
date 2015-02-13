module Sq.Parser where

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Language as L
import Control.Applicative ((<*), (*>))

type Var = String
type Fun = String
type Arg = String

data Query 
    = AppExpr Query Query
    | Bind Query F
    | Return Query
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

--sq :: T.LanguageDef
sqDef = L.emptyDef 
        { T.reservedNames = ["modules", "functions"]
        }

lexer = T.makeTokenParser sqDef

identifier = T.identifier lexer
symbol     = T.symbol lexer
reserved   = T.reserved lexer
braces     = T.braces lexer
whiteSpace = T.whiteSpace lexer
stringLiteral = T.stringLiteral lexer
comma      = T.comma lexer

query :: Parser Query
query = whiteSpace *> braces bind

var :: Parser Query
var = do v <- identifier
         return (VarExpr v)

app :: Parser Query
app = do f <- functions
         a <- identifier
         return (AppExpr f (VarExpr a))

-- query = { var <- query | query }

bind :: Parser Query
bind =  do 
  v <- try $ do 
          v <- identifier
          _ <- bindop
          return v
  x <- bindable
--  _ <- comma
  rest <- bindable
  return (Bind x (F v rest))

ret :: Parser Query
ret = vline *> Return `fmap` (app <|> var <|> query)

vline :: Parser String
vline = symbol "|"

bindop :: Parser String
bindop = symbol "<-"

bindable :: Parser Query
bindable = modules <|> (comma *> bind) <|> app <|> var <|> query <|> ret

modules :: Parser Query
modules = reserved "modules" `as` Modules

functions :: Parser Query
functions = reserved "functions" `as` Functions

relation :: Parser Query
relation = do a1 <- app
              rel <- relop
              a2 <- app
              return (RelExpr a1 rel a2)

relop :: Parser Binop
relop = (eq <|> lt <|> gt) <* spaces

eq :: Parser Binop
eq = string "==" `as` Eq

lt :: Parser Binop
lt = string "<" `as` Lt

gt :: Parser Binop
gt = string ">" `as` Gt

as :: Parser a -> b -> Parser b
as p x = do { _ <- p; return x }
