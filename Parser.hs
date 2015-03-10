module Parser where

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Language as L
import Control.Applicative ((<*), (*>))
import Control.Applicative ((<$>))
import Prelude hiding (filter)

import Types (UQuery(..),UF(..),UFun(..),Binop(..))

--- Parsers:

sqDef = L.haskellStyle
        { T.opStart = oneOf "<=>∪⊆∈∘"
        , T.opLetter = T.opStart sqDef
        }

lexer = T.makeTokenParser sqDef

-- lexer = L.haskell

lexeme     = T.lexeme lexer
identifier = T.identifier lexer
symbol     = T.symbol lexer
reserved   = T.reserved lexer
reservedOp = T.reservedOp lexer
braces     = T.braces lexer
whiteSpace = T.whiteSpace lexer
stringLiteral = T.stringLiteral lexer
comma      = T.comma lexer
decimal    = T.decimal lexer
parens     = T.parens lexer

query :: Parser UQuery
query = whiteSpace *> braces bind <?> "query"

ref :: Parser UQuery
ref = URef <$> try (identifier <* notFollowedBy (symbol "∘"))

app :: Parser UQuery
app = parens app
      <|>
      try (do f <- identifier
              args <- many1 (argument <|> parens argument)
              return (UAppExpr (UFName f) args))
      <?> "function application"
          where argument = numLit <|> initial <|> parens (relation <|> app) <|> ref <|> composition <|> query

infixSetOp :: String -> Parser UQuery
infixSetOp op = 
    do
      as <- try $ (query <|> initial <|> app <|> ref) <* reservedOp op
      bs <- query <|> initial <|> app
      return $ UAppExpr (UFName op) [as,bs]
    <|> parens (infixSetOp op)

union :: Parser UQuery
union = infixSetOp "∪" <?> "union"

subset :: Parser UQuery
subset = infixSetOp "⊆" <?> "subset of"

element :: Parser UQuery
element = infixSetOp "∈" <?> "element of"

funref :: Parser UFun
funref = UFName <$> identifier <?> "function reference"

ring :: Parser String
ring = lexeme $ string "∘"

{-
composition :: Parser UQuery
composition = do
  f <- try $ funref <* ring
  rest <- composition <|> (UFunExpr <$> funref)
  return $ UFunComp f rest
-}

composition :: Parser UQuery
composition = UFunComp <$> funref `sepBy1` ring <?> "function composition"

-- query = { var <- query | query }

bind :: Parser UQuery
bind =  do 
  v <- try $ identifier <* bindop
  x <- bindable
  rest <- following
  return (UBind x (UF v rest))

bindop :: Parser String
bindop = symbol "<-"

bindable :: Parser UQuery
bindable = initial <|> union <|> query <|> app

following :: Parser UQuery
following = (comma *> (bind <|> filter)) <|> (vline *> ret)

filter :: Parser UQuery
filter = do
  f <- relation <|> subset <|> element <|> app
  rest <- following
  return (UBind (UGuard f) (UF "()" rest))

vline :: Parser String
vline = symbol "|"

ret :: Parser UQuery
ret = UReturn <$> (app <|> ref <|> query)

relation :: Parser UQuery
relation = do rel <- try $ do 
                       a1 <- relOperand
                       rel <- relop
                       return $ URelation rel a1
              a2 <- relOperand
              return $ rel a2
           <?> "relation"

relOperand :: Parser UQuery
relOperand = app <|> ref <|> numLit <|> stringLit

stringLit :: Parser UQuery
stringLit = UStringLit <$> stringLiteral

numLit :: Parser UQuery
numLit = do 
  n <- lexeme decimal
  return $ UNumLit (fromIntegral n)

relop :: Parser Binop
relop = (eq <|> neq <|> lte <|> lt <|> gte <|> gt <|> regexp) <* spaces

eq :: Parser Binop
eq = try (symbol "==") `as` Eq

neq :: Parser Binop
neq = symbol "/=" `as` NEq

lt :: Parser Binop
lt = symbol "<" `as` Lt

lte :: Parser Binop
lte = try (symbol "<=") `as` Lte

gt :: Parser Binop
gt = symbol ">" `as` Gt

gte :: Parser Binop
gte = try (symbol ">=") `as` Gte

regexp :: Parser Binop
regexp = symbol "=~" `as` Regexp

initial :: Parser UQuery
initial = modules <|> atModule <|> atFile <|> atFunction <|> atExpression <?> "initial selector"

atModule :: Parser UQuery
atModule = reserved "atModule" `as` UAtModule

modules :: Parser UQuery
modules = reserved "modules" `as` UModules

atFile :: Parser UQuery
atFile = reserved "atFile" `as` UAtFile

atFunction :: Parser UQuery
atFunction = reserved "atFunction" `as` UAtFunction

atExpression :: Parser UQuery
atExpression = reserved "atExpression" `as` UAtExpr
         
as :: Parser a -> b -> Parser b
as p x = do { _ <- p; return x }

