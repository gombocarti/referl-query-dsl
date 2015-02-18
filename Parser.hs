{-# LANGUAGE KindSignatures, GADTs #-}
module Parser where

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Language as L
import Control.Applicative ((<*), (*>))
import qualified Sq

type Var = String

class Wrap a where
    wrap :: a -> Value

data Value
    = Module Sq.DbModule
    | Function Sq.DbFunction
    | String String
    | Int Int
    | Bool Bool
    | Unit
    | Seq [Value]
      deriving Eq

data TQuery :: * -> * where
              TAppExpr :: TQuery (a -> b) -> TQuery a -> TQuery b
              TBind  :: Wrap a => TQuery [a] -> TF (a -> [b]) -> TQuery [b]
              TReturn :: TQuery a -> TQuery [a]
              TUnionExpr :: TQuery a -> TQuery a -> TQuery a
              TVarExpr :: Var -> TQuery a
              TGuard :: TQuery Bool -> TQuery [()]
              TRelation :: Ord a => TQuery a -> Binop -> TQuery a -> TQuery Bool
              TStringLit ::  String -> TQuery String
              TModules :: TQuery [Sq.DbModule]
              TFunctions :: TQuery (Sq.DbModule -> [Sq.DbFunction])
              TName :: Sq.Named a => TQuery (a -> String)
              TArity :: TQuery (Sq.DbFunction -> Int)
              TUnit :: TQuery ()

data UQuery
    = UAppExpr UQuery UQuery
    | UBind UQuery UF
    | UReturn UQuery
    | UUnionExpr UQuery UQuery
    | UVarExpr Var
    | UGuard UQuery
    | URelation UQuery Binop UQuery
    | UStringLit String
    | UNumLit Int
    | UModules
    | UFunctions
    | UName
    | UArity
      deriving Show

data TF :: * -> * where
           TF :: TQuery a -> TQuery b -> TF (a -> b)


data UF = UF Var UQuery
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

query :: Parser UQuery
query = whiteSpace *> braces bind

var :: Parser UQuery
var = do v <- identifier
         return (UVarExpr v)

app :: Parser UQuery
app = do f <- functions
         a <- identifier
         return (UAppExpr f (UVarExpr a))

-- query = { var <- query | query }

bind :: Parser UQuery
bind =  do 
  v <- try $ identifier <* bindop
  x <- bindable
  rest <- following
  return (UBind x (UF v rest))

ret :: Parser UQuery
ret = vline *> UReturn `fmap` (app <|> var <|> query)

vline :: Parser String
vline = symbol "|"

bindop :: Parser String
bindop = symbol "<-"

bindable :: Parser UQuery
bindable = modules <|> app <|> query

following :: Parser UQuery
following = (comma *> (relation <|> bind)) <|> ret

modules :: Parser UQuery
modules = reserved "modules" `as` UModules

functions :: Parser UQuery
functions = reserved "functions" `as` UFunctions

name :: Parser UQuery
name = do 
  try $ do _ <- string "name" 
           notFollowedBy letter
  spaces
  v <- var
  return (UAppExpr UName v)

arity :: Parser UQuery
arity = do 
  try $ do _ <- string "arity" 
           notFollowedBy letter
  spaces
  v <- var
  return (UAppExpr UArity v)

relation :: Parser UQuery
relation = do a1 <- (predicate <|> (fmap UStringLit stringLiteral))
              rel <- relop
              a2 <- (predicate <|> (fmap UStringLit stringLiteral))
              rest <- following
              return (UBind (UGuard (URelation a1 rel a2)) (UF "()" rest))

predicate :: Parser UQuery
predicate = name <|> arity


relop :: Parser Binop
relop = (eq <|> lt <|> gt) <* spaces

eq :: Parser Binop
eq = symbol "==" `as` Eq

lt :: Parser Binop
lt = symbol "<" `as` Lt

gt :: Parser Binop
gt = symbol ">" `as` Gt

as :: Parser a -> b -> Parser b
as p x = do { _ <- p; return x }

