{-# LANGUAGE GADTs, ExistentialQuantification #-}
module Parser where

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Language as L
import Control.Applicative ((<*), (*>))
import qualified Sq (DbModule, DbFunction, Named, FileType)
import Control.Monad.Error (throwError)
import Control.Applicative ((<$>))
import Prelude hiding (filter)

-- |Identifiers.
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

-- |Untyped syntax tree type for queries.
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
    | UFiles
    | AtFile
    | AtFunction
    | AtExpr
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
    | UCalls
    | UNull      -- ^ Prelude.null.
    | UNot       -- ^ Prelude.not
    | UFName String -- ^ Function identified by its name.
    | UExported
    | URecursivity
    | UReturns
    | UReferences
    | UParameters
      deriving (Show, Eq)

-- |Untyped function.
data UF = UF Id UQuery
          deriving Show

-- |Types of the query language.
data Typ
    = List Typ
    | Poly -- ^ Polymorphic types.
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
    | FunRecursivity
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
check UFiles _env = return $ UFiles ::: List File
check AtFile _env = return $ AtFile ::: File
check AtFunction _env = return $ AtFunction ::: Fun
check AtExpr _env = return $ AtExpr ::: Expr
check (UAppExpr (UFName f) arg) env = do
  arg' ::: argt <- check arg env
  (f', ft) <- checkFun f argt
  return $ (UAppExpr f' arg') ::: ft
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
checkFun "name" p = named p >> return  (UName, String)
checkFun "references" p = referencable p >> return (UReferences, List Expr)
checkFun f p = case lookup f funtypes of
                 Just (f', pt, rt) -> do
                   expect pt p;
                   return (f', rt)
                 Nothing -> throwError $ "undefined function: " ++ f

-- |Stores name, ast node, parameter type, return type of functions.
funtypes :: [(Id, (UFun, Typ, Typ))]
funtypes = [("functions", (UFunctions, Mod, List Fun))
           , ("arity", (UArity, Fun, Int))
           , ("null", (UNull, List Poly, Bool))
           , ("calls", (UCalls, Fun, List Fun))
           , ("path", (UPath, File, String))
           , ("file", (UFile, Mod, List File))
           , ("exported", (UExported, Fun, Bool))
           , ("recursivity", (URecursivity, Fun, FunRecursivity))
           , ("returns", (UReturns, Fun, Type))
           , ("parameters", (UParameters, Fun, List Expr))
           , ("not", (UNot, Bool, Bool))
           ]

-- |Checks whether the particular type have name function.
named :: Typ -> Either String ()
named t | t `elem` [File,Mod,Fun,Record] = return ()
        | otherwise = throwError $ "dont have name: " ++ show t

referencable :: Typ -> Either String ()
referencable t | t `elem` [Fun,Record] = return ()
               | otherwise = throwError $ "not referencable :" ++ show t

expect :: Typ -> Typ -> Either String ()
expect (List Poly) (List _b) = return ()
expect (List _a) (List Poly) = return ()
expect exp act | act == exp = return ()
               | otherwise = throwError $ "type error: expected: " ++ show exp ++ ", actual: " ++ show act

data Binop
    = Eq
    | NEq
    | Lt
    | Lte
    | Gt
    | Gte
      deriving Show

--- Parsers:

sqDef = L.haskellDef
        { T.opStart = oneOf "<=>"
        , T.opLetter = T.opStart sqDef
        }

lexer = T.makeTokenParser sqDef

-- lexer = L.haskell

lexeme     = T.lexeme lexer
identifier = T.identifier lexer
symbol     = T.symbol lexer
reserved   = T.reserved lexer
braces     = T.braces lexer
whiteSpace = T.whiteSpace lexer
stringLiteral = T.stringLiteral lexer
comma      = T.comma lexer
decimal    = T.decimal lexer
parens     = T.parens lexer

query :: Parser UQuery
query = whiteSpace *> braces bind

var :: Parser UQuery
var = UVarExpr <$> identifier

app :: Parser UQuery
app = parens app
      <|> 
      (try $ do f <- identifier
                arg <- var <|> app
                return (UAppExpr (UFName f) arg))

-- query = { var <- query | query }

bind :: Parser UQuery
bind =  do 
  v <- try $ identifier <* bindop
  x <- bindable
  rest <- following
  return (UBind x (UF v rest))

ret :: Parser UQuery
ret = vline *> (UReturn <$> (app <|> var <|> query))

vline :: Parser String
vline = symbol "|"

bindop :: Parser String
bindop = symbol "<-"

bindable :: Parser UQuery
bindable = modules <|> app <|> query

following :: Parser UQuery
following = (comma *> (filter <|> bind)) <|> ret

modules :: Parser UQuery
modules = reserved "modules" `as` UModules

relation :: Parser UQuery
relation = do rel <- try $ do 
                       a1 <- relOperand
                       rel <- relop
                       return $ URelation rel a1
              a2 <- relOperand
              return $ rel a2

filter :: Parser UQuery
filter = do 
  f <- relation <|> app
  rest <- following
  return (UBind (UGuard f) (UF "()" rest))

relOperand :: Parser UQuery
relOperand = app <|> var <|> numLit <|> stringLit

stringLit :: Parser UQuery
stringLit = UStringLit <$> stringLiteral

numLit :: Parser UQuery
numLit = do 
  n <- lexeme decimal
  return $ UNumLit (fromIntegral n)

relop :: Parser Binop
relop = (eq <|> neq <|> lte <|> lt <|> gte <|> gt) <* spaces

eq :: Parser Binop
eq = symbol "==" `as` Eq

neq :: Parser Binop
neq = symbol "/=" `as` NEq

lt :: Parser Binop
lt = symbol "<" `as` Lt

lte :: Parser Binop
lte = symbol "<=" `as` Lte

gt :: Parser Binop
gt = symbol ">" `as` Gt

gte :: Parser Binop
gte = symbol ">=" `as` Gte

relations :: [(String, Binop)]
relations = [("==", Eq), ("<", Lt), ("<=", Lte), (">", Gt), (">=", Gte), ("/=", NEq)]

as :: Parser a -> b -> Parser b
as p x = do { _ <- p; return x }

