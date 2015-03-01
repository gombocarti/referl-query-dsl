{-# LANGUAGE GADTs, ExistentialQuantification #-}
module Parser where

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Language as L
import Control.Applicative ((<*), (*>))
import qualified Sq (DbModule, DbFunction, Named, FileType)
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

-- |Applicable functions of the query language.
data UFun
    = UFunctions -- ^ Functions of a module.
    | UPath      -- ^ Path of a loaded file.
    | UIsModule  -- ^ True if the file contains a module.
    | UFile      -- ^ File of a module.
    | UExports   -- ^ Exported functions of a module.
    | UImports   -- ^ Imported functions of a module.
    | ULoc       -- ^ Line of code.
    | UName      
    | UArity
    | UFName String -- ^ Function identified by its name.
      deriving (Show, Eq)

-- |Untyped function.
data UF = UF Id UQuery
          deriving Show

-- |Types of the query language.
data Typ
    = List Typ
    | File
    | Mod
    | Fun
    | Expr
    | Macro
    | Record
    | Spec
    | SpecParam
    | Type
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

data TUQuery = UQuery ::: Typ deriving Show

check :: UQuery -> TEnv -> Either String TUQuery
check (UBind m (UF x body)) e = do
  m' ::: List tm <- check m e
  body' ::: List tbody <- check body ((x,tm):e)
  return $ (UBind m' (UF x body')) ::: List tbody
check (UReturn x) e = do
  x' ::: t <- check x e
  return $ UReturn x' ::: List t
check (UVarExpr v) e = do
  tv <- getVar e v  
  return $ UVarExpr v ::: tv
check UModules _env = return $ UModules ::: List Mod
check (UAppExpr (UFName f) (UVarExpr v)) env = do
  vt <- getVar env v
  (f', ft) <- checkFun f vt
  return $ (UAppExpr f' (UVarExpr v)) ::: ft                   
check (URelation op q1 q2) env = do
  q1' ::: t1 <- check q1 env
  q2' ::: t2 <- check q2 env
  expect t1 t2
  return $ (URelation op q1' q2') ::: Bool
check (UGuard p) env = do
  p' ::: t <- check p env
  expect Bool t
  return $ p' ::: List Unit
check q@(UNumLit _) _env = return $ q ::: Int
check q@(UStringLit _) _env = return $ q ::: String
check (UUnionExpr q1 q2) env = do
  q1' ::: t1 <- check q1 env
  q2' ::: t2 <- check q2 env
  expect t1 t2
  return $ (UUnionExpr q1' q2') ::: t1

checkFun :: Id -> Typ -> Either String (UFun, Typ)
checkFun f p | f == "name" = named p >> return  (UName, String)
             | otherwise = case lookup f funtypes of
                 Just (f', pt, rt) -> do
                   expect pt p;
                   return (f', rt)
                 Nothing -> throwError $ "undefined function: " ++ f

-- |Stores name, ast node, parameter type, return type of functions.
funtypes :: [(Id, (UFun, Typ, Typ))]
funtypes = [("functions", (UFunctions, Mod, List Fun)), ("arity", (UArity, Fun, Int))]

-- |Checks whether the particular type have name function.
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

{-
sqDef = L.emptyDef 
        { T.reservedNames = ["modules", "functions"]
        }

lexer = T.makeTokenParser sqDef
-}

lexer = L.haskell

lexeme     = T.lexeme lexer
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
app = try $ do f <- identifier
               a <- identifier
               return (UAppExpr (UFName f) (UVarExpr a))

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

