{-# LANGUAGE GADTs, ExistentialQuantification #-}
module Parser where

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Language as L
import Control.Applicative ((<*), (*>))
import qualified Sq
import Control.Monad.Error (throwError)

type Id = String

data TQuery a where
    TAppExpr :: TQuery (a -> b) -> TQuery a -> TQuery b
    TBind  :: TQuery [a] -> TF (a -> [b]) -> TQuery [b]
    TReturn :: TQuery a -> TQuery [a]
    TUnionExpr :: TQuery [a] -> TQuery [a] -> TQuery [a]
    TVarExpr :: Id -> TQuery a
    TGuard :: TQuery Bool -> TQuery [()]
    TRelation :: Ord a => TQuery a -> Binop -> TQuery a -> TQuery Bool
    TStringLit ::  String -> TQuery String
    TModules :: TQuery [Sq.DbModule]
    TFunctions :: TQuery (Sq.DbModule -> [Sq.DbFunction])
    TName :: Sq.Named a => TQuery (a -> String)
    TArity :: TQuery (Sq.DbFunction -> Int)
    TUnit :: TQuery ()

data TF a where
    TF :: TQuery a -> TQuery b -> TF (a -> b)

data UQuery
    = UAppExpr UFun UQuery
    | UBind UQuery UF
    | UReturn UQuery
    | UUnionExpr UQuery UQuery
    | UVarExpr Id
    | UGuard UQuery
    | URelation Binop UQuery UQuery
    | UStringLit String
    | UNumLit Int
    | UModules
      deriving Show

data UFun
    = UFunctions
    | UName
    | UArity
      deriving Show

data UF = UF Id UQuery
          deriving Show

data Typ
    = List Typ
    | Mod
    | Fun
    | String
    | Int
    | Bool
    | Unit
      deriving (Show,Eq)

getVar :: TEnv -> Id -> Either String Typ
getVar env v = case lookup v env of
                 Just x  -> return x
                 Nothing -> throwError $ "unbound variable: " ++ v

type TEnv = [(Id,Typ)]

check :: UQuery -> TEnv -> Either String Typ
check (UBind m (UF x body)) e = do
  List tm <- check m e
  List tbody <- check body ((x,tm):e)
  return $ List tbody
check (UReturn x) e = do
  t <- check x e
  return $ List t
check (UVarExpr v) e = getVar e v  
check UModules _env = return $ List Mod
check (UAppExpr UName (UVarExpr v)) e = do
  t <- getVar e v 
  named t
  return String
check (UAppExpr UArity (UVarExpr v)) e = do
  t <- getVar e v
  expect Fun t
  return Int
check (UAppExpr UFunctions (UVarExpr v)) env = do
  t <- getVar env v
  expect t Mod
  return Fun
check (URelation _op q1 q2) env = do
  t1 <- check q1 env
  t2 <- check q2 env
  expect t1 t2
  return Bool
check (UGuard p) env = do
  t <- check p env
  expect Bool t
  return $ List Unit
check (UNumLit _) _env = return Int
check (UStringLit _) _env = return String
check (UUnionExpr q1 q2) env = do
  t1 <- check q1 env
  t2 <- check q2 env
  expect t1 t2
  return t1

named :: Typ -> Either String ()
named t | t `elem` [Mod,Fun] = return ()
        | otherwise = throwError $ "dont have name: " ++ show t

expect :: Typ -> Typ -> Either String ()
expect exp act | act == exp = return ()
               | otherwise = throwError $ "type error: expected: " ++ show exp ++ ", actual: " ++ show act

data Binop
    = Eq
    | Lt
    | Lte
    | Gt
    | Gte
      deriving Show

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
var = UVarExpr `fmap` identifier

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

functions :: Parser UFun
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
              return (UBind (UGuard (URelation rel a1 a2)) (UF "()" rest))

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

