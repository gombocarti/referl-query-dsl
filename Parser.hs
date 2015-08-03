module Parser where

import Text.Parsec
import Text.Parsec.Pos (initialPos)
import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Language as L
import Control.Applicative ((<*), (*>))
import Control.Applicative ((<$>))
import Control.Monad.Identity
import Control.Monad.IO.Class (liftIO)
import Prelude hiding (filter)
import Data.Maybe (fromJust)

import Types (Id, UQuery(..),UF(..))

-- | Convenience function for parsing queries.
parseQuery :: String -> IO (Either ParseError UQuery)
parseQuery s = runParserT query Nothing "" s

--- Parsers:

sqDef :: Monad m => L.GenLanguageDef String u m
sqDef = T.LanguageDef
        { T.identStart = lower
        , T.identLetter = alphaNum <|> oneOf "'_"
        , T.opStart = oneOf "<=>∪⊆∈∘"
        , T.reservedOpNames = []
        , T.opLetter = T.opStart sqDef
        , T.reservedNames = ["groupBy","with"]
        , T.caseSensitive = True
        , T.commentLine = "--"
        , T.commentStart = "{-"
        , T.commentEnd = "-}"
        , T.nestedComments = True
        }

lexer :: Monad m => T.GenTokenParser String u m
lexer = T.makeTokenParser sqDef

-- lexer = L.haskell

braces,lexeme,parens :: QParser a -> QParser a
commaSep1 :: QParser a -> QParser [a]
comma :: QParser String
identifier,stringLiteral :: QParser String
symbol :: String -> QParser String
reserved :: String -> QParser ()
reservedOp :: String -> QParser ()
whiteSpace :: QParser ()
decimal :: QParser Integer

lexeme        = T.lexeme lexer
identifier    = T.identifier lexer
symbol        = T.symbol lexer
reserved      = T.reserved lexer
reservedOp    = T.reservedOp lexer
braces        = T.braces lexer
whiteSpace    = T.whiteSpace lexer
stringLiteral = T.stringLiteral lexer
comma         = T.comma lexer
decimal       = T.decimal lexer
parens        = T.parens lexer
commaSep1     = T.commaSep1 lexer

type QParser a = ParsecT String (Maybe UQuery) IO a

set :: QParser UQuery
set = braces q <?> "query"
    where q = do
            r <- ret
            _ <- vline
            x <- getState
            putState (Just r)
            b <- compr_elem
            putState x
            return b

compr_elem :: QParser UQuery
compr_elem = bind <|> filter

groupby :: QParser UQuery
groupby = do
  try $ reserved "groupBy"
  f <- identifier
  q <- set
  return $ UAppExpr (UAppExpr (URef "groupBy") (URef f)) q

query :: QParser UQuery
query = do 
  q <- whiteSpace *> (set <|> initial <|> groupby <|> with <|> relation <|> app <|> ref)
  return (UQuery q)

ref :: QParser UQuery
ref = URef <$> try (name <* notFollowedBy (symbol "∘"))

dataConst :: QParser UQuery
dataConst = UDataConst <$> cons
    where cons = lexeme $ do 
                   c <- upper
                   s <- many (alphaNum <|> char '_')
                   return $ c:s

app :: QParser UQuery
app = parens app
      <|>
      try (do f <- identifier
              args <- many1 (argument <|> parens argument)
              return (foldl UAppExpr (URef f) args))
              <* whiteSpace
      <?> "function application"
           where argument = numLit <|> stringLit <|> initial <|> parens (relation <|> composition <|> app) <|> ref <|> set

infixSetOp :: String -> QParser UQuery
infixSetOp op = 
    do
      as <- try $ (set <|> initial <|> app <|> ref) <* reservedOp op
      bs <- set <|> initial <|> app
      return $ UAppExpr (UAppExpr (URef op) as) bs
    <|> parens (infixSetOp op)

union :: QParser UQuery
union = infixSetOp "∪" <?> "union"

subset :: QParser UQuery
subset = infixSetOp "⊆" <?> "subset of"

element :: QParser UQuery
element = infixSetOp "∈" <?> "element of"

funref :: QParser UQuery
funref = URef <$> name <?> "function reference"

name :: QParser String
name = do
  c <- T.identStart sqDef
  cs <- many (T.identLetter sqDef)
  skipMany (oneOf " \t")
  return (c:cs)

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
composition = do
  first <- try ((app <|> funref) <* ring)
  remaining <- (app <|> funref) `sepBy1` ring
  return (UFunComp (first : remaining))
  <?> "function composition"

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
bindable = initial <|> union <|> set <|> app <|> ref

following :: QParser UQuery
following = do q <-  optionMaybe (comma *> compr_elem)
               case q of 
                 Just x  -> return x
                 Nothing -> fromJust <$> getState                       

filter :: QParser UQuery
filter = do
  f <- relation <|> subset <|> element <|> app
  rest <- following
  return (UGuard f rest)

vline :: QParser String
vline = symbol "|"

ret :: QParser UQuery
ret = UReturn <$> (tuple <|> app <|> ref <|> set <|> numLit <|> stringLit)

tuple :: QParser UQuery
tuple = parens elems <?> "tuple"
    where 
      elems = UTuple <$> commaSep1 (app <|> ref)

relation :: QParser UQuery
relation = do rel <- try $ do 
                       a1 <- relOperand
                       rel <- relop
                       return $ \op2 -> UAppExpr (UAppExpr (URef rel) a1) op2
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

relop :: QParser String
relop = (eq <|> neq <|> lte <|> lt <|> gte <|> gt <|> regexp) <* spaces

eq :: QParser String
eq = try (symbol "==")

neq :: QParser String
neq = symbol "/="

lt :: QParser String
lt = symbol "<" 

lte :: QParser String
lte = try (symbol "<=")

gt :: QParser String
gt = symbol ">"

gte :: QParser String
gte = try (symbol ">=")

regexp :: QParser String
regexp = symbol "=~"

initial :: QParser UQuery
initial = modules <|> files <|> atModule <|> atFile <|> atFunction <|> atExpression <|> atField <?> "initial selector"

atModule :: QParser UQuery
atModule = reserved "atModule" *> return (URef "atModule")

atField :: QParser UQuery
atField = reserved "atField" *> return (URef "atField")

modules :: QParser UQuery
modules = reserved "modules" *> return (URef "modules")

files :: QParser UQuery
files = reserved "files" *> return (URef "files")

atFile :: QParser UQuery
atFile = reserved "atFile" *> return (URef "atFile")

atFunction :: QParser UQuery
atFunction = reserved "atFunction" *> return (URef "atFunction")

atExpression :: QParser UQuery
atExpression = reserved "atExpression" *> return (URef "atExpression")
         
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
  pos <- getPosition
  return $ UFunDef f args body pos

parseFile :: FilePath -> QParser [UQuery]
parseFile file = do
  st <- getParserState
  contents <- liftIO (readFile file)
  setInput contents
  setPosition (initialPos file)
  defs <- whiteSpace *> many def
  _ <- setParserState st
  return defs
