module Parser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Pos (newPos)
import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Language as L
import Control.Applicative ((<*), (*>))
import Control.Applicative ((<$>))
import Control.Monad.Identity
import Control.Monad.IO.Class (liftIO)
import Prelude hiding (filter)
import Data.Maybe (fromJust)

import Types (UQuery(..),UF(..),UFun(..),Binop(..))

--- Parsers:

sqDef :: Monad m => L.GenLanguageDef String u m
sqDef = T.LanguageDef
        { T.identStart = lower
        , T.identLetter = alphaNum <|> oneOf "'_"
        , T.opStart = oneOf "<=>∪⊆∈∘"
        , T.reservedOpNames = []
        , T.opLetter = T.opStart sqDef
        , T.reservedNames = ["group","with"]
        , T.caseSensitive = True
        , T.commentLine = "--"
        , T.commentStart = "{-"
        , T.commentEnd = "-}"
        , T.nestedComments = True
        }

lexer :: Monad m => T.GenTokenParser String u m
lexer = T.makeTokenParser sqDef

-- lexer = L.haskell

braces :: QParser a -> QParser a

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
commaSep1  = T.commaSep1 lexer

-- type QParser a = ParsecT String (Maybe UQuery) IO a

type QParser a = ParsecT String (Maybe UQuery) IO a

{-             
query :: QParser UQuery
query = braces bind <?> "query"
-}

set :: QParser UQuery
set = braces q <?> "query"
    where q = do
            r <- ret
            _ <- vline
            x <- getState
            putState . Just $ r
            b <- bind
            putState x
            return b

aggregation :: QParser UQuery
aggregation = do
  f <- identifier
  q <- set
  return $ UAppExpr (UFName f) [q]

groupby :: QParser UQuery
groupby = do
  try $ reserved "group"
  f <- identifier
  q <- set
  return $ UGroupBy (UFName f) q

query :: QParser UQuery
query = whiteSpace *> (set <|> initial <|> groupby <|> with <|> aggregation)

ref :: QParser UQuery
ref = URef <$> try (identifier <* notFollowedBy (symbol "∘"))

dataConst :: QParser UQuery
dataConst = UDataConst <$> cons
    where cons = lexeme $ do 
                   c <- upper
                   s <- many alphaNum
                   return $ c:s

app :: QParser UQuery
app = parens app
      <|>
      try (do f <- identifier
              args <- many1 (argument <|> parens argument)
              return (UAppExpr (UFName f) args))
      <?> "function application"
          where argument = numLit <|> initial <|> parens (relation <|> app) <|> ref <|> composition <|> set

infixSetOp :: String -> QParser UQuery
infixSetOp op = 
    do
      as <- try $ (set <|> initial <|> app <|> ref) <* reservedOp op
      bs <- set <|> initial <|> app
      return $ UAppExpr (UFName op) [as,bs]
    <|> parens (infixSetOp op)

union :: QParser UQuery
union = infixSetOp "∪" <?> "union"

subset :: QParser UQuery
subset = infixSetOp "⊆" <?> "subset of"

element :: QParser UQuery
element = infixSetOp "∈" <?> "element of"

funref :: QParser UFun
funref = UFName <$> identifier <?> "function reference"

ring :: QParser String
ring = lexeme $ string "∘"

{-
composition :: QParser UQuery
composition = do
  f <- try $ funref <* ring
  rest <- composition <|> (UFunExpr <$> funref)
  return $ UFunComp f rest
-}

composition :: QParser UQuery
composition = UFunComp <$> funref `sepBy1` ring <?> "function composition"

-- query = { var <- query | query }

bind :: QParser UQuery
bind =  do 
  v <- try $ identifier <* bindop
  x <- bindable
  rest <- following
  return (UBind x (UF v rest))

bindop :: QParser String
bindop = symbol "<-"

bindable :: QParser UQuery
bindable = initial <|> union <|> set <|> app

following :: QParser UQuery
following = do q <-  optionMaybe (comma *> (bind <|> filter))
               case q of 
                 Just x  -> return x
                 Nothing -> fromJust <$> getState                       

filter :: QParser UQuery
filter = do
  f <- relation <|> subset <|> element <|> app
  rest <- following
  return (UBind (UGuard f) (UF "()" rest))

vline :: QParser String
vline = symbol "|"

ret :: QParser UQuery
ret = UReturn <$> (tuple <|> app <|> ref <|> set)

tuple :: QParser UQuery
tuple = parens elems <?> "tuple"
    where 
      elems = UTuple <$> commaSep1 ref

relation :: QParser UQuery
relation = do rel <- try $ do 
                       a1 <- relOperand
                       rel <- relop
                       return $ URelation rel a1
              a2 <- relOperand
              return $ rel a2
           <?> "relation"

relOperand :: QParser UQuery
relOperand = app <|> ref <|> numLit <|> stringLit <|> dataConst

stringLit :: QParser UQuery
stringLit = UStringLit <$> stringLiteral

numLit :: QParser UQuery
numLit = do 
  n <- lexeme decimal
  return $ UNumLit (fromIntegral n)

relop :: QParser Binop
relop = (eq <|> neq <|> lte <|> lt <|> gte <|> gt <|> regexp) <* spaces

eq :: QParser Binop
eq = try (symbol "==") `as` Eq

neq :: QParser Binop
neq = symbol "/=" `as` NEq

lt :: QParser Binop
lt = symbol "<" `as` Lt

lte :: QParser Binop
lte = try (symbol "<=") `as` Lte

gt :: QParser Binop
gt = symbol ">" `as` Gt

gte :: QParser Binop
gte = try (symbol ">=") `as` Gte

regexp :: QParser Binop
regexp = symbol "=~" `as` Regexp

initial :: QParser UQuery
initial = modules <|> files <|> atModule <|> atFile <|> atFunction <|> atExpression <?> "initial selector"

atModule :: QParser UQuery
atModule = reserved "atModule" *> return UAtModule

modules :: QParser UQuery
modules = reserved "modules" *> return UModules

files :: QParser UQuery
files = reserved "files" *> return UFiles

atFile :: QParser UQuery
atFile = reserved "atFile" *> return UAtFile

atFunction :: QParser UQuery
atFunction = reserved "atFunction" *> return UAtFunction

atExpression :: QParser UQuery
atExpression = reserved "atExpression" *> return UAtExpr
         
as :: QParser a -> b -> QParser b
as p x = do { _ <- p; return x }

with :: QParser UQuery
with = do
  reserved "with"
  file <- stringLiteral
  defs <- parseFile file
  q <- query
  return $ UWith defs q

def :: QParser UQuery
def = do
  f <- identifier
  args <- many identifier
  _ <- symbol "="
  body <- query
  return $ UFunDef f args body

parseFile :: FilePath -> QParser [UQuery]
parseFile file = do
  st <- getParserState
  contents <- liftIO (readFile file)
  setInput contents
  let pos = newPos file 1 1
  setPosition pos
  defs <- whiteSpace *> many def
  _ <- setParserState st
  return defs
