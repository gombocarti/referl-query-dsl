module Parser (parseQuery) where

import Text.Parsec hiding (runParser)
import Text.Parsec.Pos (initialPos)
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Language as L
import Control.Applicative ((<*), (*>))
import Control.Applicative ((<$>))
import Control.Monad.Identity
import Control.Monad.IO.Class (liftIO)
import Prelude hiding (filter)
import Data.Maybe (fromJust)

import Types

-- | Convenience function for parsing queries.
parseQuery :: String -> IO (Either ParseError Query)
parseQuery s = runParserT query Nothing "" s

runParser :: QParser Query -> String -> IO (Either ParseError Query)
runParser p s = runParserT p Nothing "" s

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
identifier :: QParser String
stringLiteral :: QParser String
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

type QParser a = ParsecT String (Maybe Query) IO a

set :: QParser Query
set = braces q <?> "query"
    where q = do
            r <- ret
            _ <- vline
            x <- getState
            putState (Just r)
            b <- compr_elem
            putState x
            return b

compr_elem :: QParser Query
compr_elem = bind <|> filter

groupby :: QParser Query
groupby = do
  try $ reserved "groupBy"
  f <- identifier
  q <- set
  return $ AppE (AppE (RefE "groupBy") (RefE f)) q

query :: QParser Query
query = do 
  q <- whiteSpace *> (set <|> initial <|> groupby <|> with <|> relation <|> app <|> ref)
  return (Query q)

ref :: QParser Query
ref = RefE <$> try (name <* notFollowedBy (symbol "∘"))

dataConst :: QParser Query
dataConst = lexeme (do 
              c <- upper
              s <- many (alphaNum <|> char '_')
              return (DataConstE (c:s)))
            <?> "data constructor"

app :: QParser Query
app = parens app
      <|>
      try (do f <- identifier
              args <- many1 (argument <|> parens argument)
              return (foldl AppE (RefE f) args))
              <* whiteSpace
      <?> "function application"
           where argument = numLit <|> stringLit <|> initial <|> parens (relation <|> composition <|> app) <|> ref <|> set

infixSetOp :: String -> QParser Query
infixSetOp op = 
    do
      as <- try $ (set <|> initial <|> app <|> ref) <* reservedOp op
      bs <- set <|> initial <|> app
      return $ AppE (AppE (RefE op) as) bs
    <|> parens (infixSetOp op)

union :: QParser Query
union = infixSetOp "∪" <?> "union"

subset :: QParser Query
subset = infixSetOp "⊆" <?> "subset of"

element :: QParser Query
element = infixSetOp "∈" <?> "element of"

funref :: QParser Query
funref = RefE <$> name <?> "function reference"

name :: QParser String
name = do
  c <- T.identStart sqDef
  cs <- many (T.identLetter sqDef)
  skipMany (oneOf " \t")
  return (c:cs)

ring :: QParser String
ring = lexeme $ string "∘"

{-
composition :: QParser Query
composition = do
  f <- try $ funref <* ring
  rest <- composition <|> (UFunExpr <$> funref)
  return $ UFunComp f rest
-}

term :: QParser Query
term = parens expr <|> app <|> ref <|> numLit <|> stringLit <|> dataConst

expr :: QParser Query
expr = buildExpressionParser table term

table :: [[Operator String (Maybe Query) IO Query]]
table = 
    [ [ binop ">" AssocRight,  binop ">=" AssocRight
      , binop "==" AssocRight, binop "/=" AssocRight
      , binop "<" AssocRight,  binop "<=" AssocRight
      , binop "~=" AssocRight
      ]
    , [binop "||" AssocRight]
    ]

binop :: String -> Assoc -> Operator String (Maybe Query) IO Query
binop op assoc = Infix p assoc
    where p = do 
            reservedOp op
            return (\x y -> AppE (AppE (RefE op) x) y)

composition :: QParser Query
composition = do
  first <- try ((app <|> funref) <* ring)
  remaining <- (app <|> funref) `sepBy1` ring
  return (FunCompE (first : remaining))
  <?> "function composition"

-- query = { var <- query | query }

bind :: QParser Query
bind =  do 
  v <- try $ identifier <* bindop
  x <- bindable
  rest <- following
  return (BindE x (Lambda v rest))

bindop :: QParser String
bindop = symbol "<-"

bindable :: QParser Query
bindable = initial <|> union <|> set <|> app <|> ref

following :: QParser Query
following = do q <-  optionMaybe (comma *> compr_elem)
               case q of 
                 Just x  -> return x
                 Nothing -> fromJust <$> getState                       

filter :: QParser Query
filter = do
  f <- expr <|> subset <|> element <|> app
  return (GuardE f)

vline :: QParser String
vline = symbol "|"

ret :: QParser Query
ret = ReturnE <$> (tuple <|> app <|> ref <|> set <|> numLit <|> stringLit)

tuple :: QParser Query
tuple = parens elems <?> "tuple"
    where 
      elems = TupleE <$> commaSep1 (app <|> ref)

relation :: QParser Query
relation = do rel <- try $ do 
                       a1 <- relOperand
                       rel <- relop
                       return $ \op2 -> AppE (AppE (RefE rel) a1) op2
              a2 <- relOperand
              return $ rel a2
           <?> "relation"

relOperand :: QParser Query
relOperand = app <|> ref <|> numLit <|> stringLit <|> dataConst

stringLit :: QParser Query
stringLit = StringLitE <$> stringLiteral

numLit :: QParser Query
numLit = do 
  n <- lexeme decimal
  return $ NumLitE (fromIntegral n)

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

initial :: QParser Query
initial = modules <|> files <|> atModule <|> atFile <|> atFunction <|> atExpression <|> atField <?> "initial selector"

atModule :: QParser Query
atModule = reserved "atModule" *> return (RefE "atModule")

atField :: QParser Query
atField = reserved "atField" *> return (RefE "atField")

modules :: QParser Query
modules = reserved "modules" *> return (RefE "modules")

files :: QParser Query
files = reserved "files" *> return (RefE "files")

atFile :: QParser Query
atFile = reserved "atFile" *> return (RefE "atFile")

atFunction :: QParser Query
atFunction = reserved "atFunction" *> return (RefE "atFunction")

atExpression :: QParser Query
atExpression = reserved "atExpression" *> return (RefE "atExpression")
         
as :: QParser a -> b -> QParser b
as p x = do { _ <- p; return x }

with :: QParser Query
with = do
  reserved "with"
  file <- stringLiteral
  defs <- parseFile file
  q <- query
  return $ WithE defs q

def :: QParser Query
def = do
  f <- identifier
  args <- many identifier
  _ <- symbol "="
  body <- query
  pos <- getPosition
  return $ FunDefE f args body pos

parseFile :: FilePath -> QParser [Query]
parseFile file = do
  st <- getParserState
  contents <- liftIO (readFile file)
  setInput contents
  setPosition (initialPos file)
  defs <- whiteSpace *> many def
  _ <- setParserState st
  return defs
