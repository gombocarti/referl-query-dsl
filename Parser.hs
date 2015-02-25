{-# LANGUAGE GADTs, ExistentialQuantification #-}
module Parser where

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Language as L
import Control.Applicative ((<*), (*>))
import qualified Sq
import Data.Maybe (isJust)

type Id = String

class Wrap a where
    wrap :: a -> Value
    unwrap :: Value -> a


data Value
    = Module Sq.DbModule
    | Function Sq.DbFunction
    | String String
    | Int Int
    | Bool Bool
    | Unit
    | Seq [Value]
      deriving Eq

data TQuery a where
    TAppExpr :: TQuery (a -> b) -> TQuery a -> TQuery b
    TBind  :: Wrap a => TQuery [a] -> TF (a -> [b]) -> TQuery [b]
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
    = UAppExpr UQuery UQuery
    | UBind UQuery UF
    | UReturn UQuery
    | UUnionExpr UQuery UQuery
    | UVarExpr Id
    | UGuard UQuery
    | URelation UQuery Binop UQuery
    | UStringLit String
    | UNumLit Int
    | UModules
    | UFunctions
    | UName
    | UArity
      deriving Show

data UF = UF Id UQuery
          deriving Show

data VType = TModule | TFunction | TInt | TString | TList VType

data Var e a where
    Zro :: Var (e,a) a
    Suc :: Var e a -> Var (e,b) a

data Env e where
    Emp :: Env ()
    Ext :: Env e -> Id -> Typ a -> Env (e,a)

data Typ a where
--    Int ::                     Typ Int
--    String :: Typ String
    List :: Typ a -> Typ [a]
    Mod :: Typ Sq.DbModule
    Fun :: Typ Sq.DbFunction
  --  Arr :: Typ a -> Typ b -> Typ (a -> b)

{-
data CompQuery t env where
    (:::) :: (env -> a) -> t a -> CompQuery t env
-}

type Env' a = [(Id, a)]

{-    
type SymTab t env = Env' (CompQuery t env)
-}
data TTQuery where
    (:::) :: TQuery a -> Typ a -> TTQuery

{-
getVar :: Env e -> Id -> Var (a,b) b
getVar (Ext e id t) v | v == id = Zro
                      | otherwise = Suc (getVar e v)

-}
{-           
getEnv :: Env e -> Id -> Typ a
getEnv Emp v = error $ "undefined variable: " ++ v
getEnv (Ext e id t) v = t
-}
check :: UQuery -> Env e -> Either String (TTQuery)
check (UBind m (UF x body)) e = do
  m' ::: List tm <- check m e
  body' ::: List tbody <- check m (Ext e x tm)
  return $ (TBind m' (TF (TVarExpr x) body')) ::: List tbody
check (UReturn x) e = do
  x' ::: tx <- check x e
  return $ (TReturn x') ::: List tx
check (UVarExpr v) e = undefined
  
  


{-
check :: UQuery -> SymTab Typ env -> Either String (CompQuery Typ env)
check (UBind m (UF x body)) e = do
  m' ::: List tm <- check m e
  body' ::: t@(List tbody) <- check body ((x,
  return $ (TBind m' (TF (TVarExpr x) body')) ::: List tbody
-}


{-           
check :: UQuery -> Env -> Maybe (ATExp)
check (UBind m (UF x body)) env = do 
  m' ::: TTList z <- check m env
  body' ::: TTList y <- check body ((x, TVarExpr x ::: z):env)
  return ((TBind m' (TF (TVarExpr x) body')) ::: TTList y)
check UModules _env = return $ TModules ::: (TTList TTMod)
-- check UName _env = return $ Sq.name ::: TTArr
check (UAppExpr UName (UVarExpr v)) env = case isDefined v env of
                                            True -> return $ TAppExpr (TName :: TQuery (Sq.DbFunction -> String)) (TVarExpr v) ::: TTStr
                                            False -> error $ "unbound variable: " ++ v
check (UAppExpr UFunctions (UVarExpr v)) env = case isDefined v env of
                                                 True -> return $ TAppExpr TFunctions (TVarExpr v) ::: TTList TTFun
                                                 False -> Nothing
check (UReturn m) env = do
  m' ::: t <- check m env
  return $ TReturn m' ::: TTList t
-}

{-
check :: Wrap a => UQuery -> Env -> Either String (TQuery [a], VType)
check (UBind m (UF x body)) env = do
  (m', mt) <- check m env
  (body', bodyt) <- check body ((x,mt):env)
  return $ (TBind m' (TF (TVarExpr x) body'), bodyt)
-}
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

