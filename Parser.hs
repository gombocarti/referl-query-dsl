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
    = UAppExpr UFun [UQuery]
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
    | UAtFile
    | UAtFunction
    | UAtExpr
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
    | UElem      -- ^ Prelude.elem 
    | UFName String -- ^ Function identified by its name.
    | UExported
    | URecursivity
    | UReturns
    | UReferences
    | UParameters
      deriving (Show, Eq)

data UFunTyp 
    = Un (Typ -> Either String Typ)
    | Bin (Typ -> Typ -> Either String Typ)

-- data UFunArity = Un | Bin deriving Show

data TErr = TErr Typ Typ

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
    | FunRecursivity
    | FilePath
      deriving (Show,Eq)

runchk :: String -> Parser UQuery -> TEnv ->  Either String TUQuery
runchk s parser env = case parse parser "" s of
                        Right x -> check x env
                        Left err -> throwError . show $ err

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
check UAtFile _env = return $ UAtFile ::: File
check UAtFunction _env = return $ UAtFunction ::: Fun
check UAtExpr _env = return $ UAtExpr ::: Expr
check (UAppExpr (UFName f) args) env = do
  targs' <- mapM (flip check env) args
  let (args', argts') = unzip [(arg, argt) | arg ::: argt <- targs']
  (f', ft) <- checkFun f argts'
  return $ (UAppExpr f' args') ::: ft
check (URelation op q1 q2) env = do
  q1' ::: t1 <- check q1 env
  q2' ::: t2 <- check q2 env
  expect t1 t2
  return $ (URelation op q1' q2') ::: Bool
check (UGuard p) env = do
  p' ::: t <- check p env
  expect Bool t
  return $ UGuard p' ::: List Unit
check q@(UNumLit _) _env = return $ q ::: Int
check q@(UStringLit _) _env = return $ q ::: String
check (UUnionExpr q1 q2) env = do
  q1' ::: t1 <- check q1 env
  q2' ::: t2 <- check q2 env
  expect t1 t2
  return $ (UUnionExpr q1' q2') ::: t1

checkFun :: Id -> [Typ] -> Either String (UFun, Typ)
checkFun "name" [p] = named p >> return  (UName, String)
checkFun "name" _   = throwError $ "too many parameters: name"
checkFun "references" [p] = referencable p >> return (UReferences, List Expr)
checkFun "references" _   = throwError $ "too many parameters: references"
checkFun f xs = case lookup f funtypes of
                  Just (f', Un chk) -> case xs of
                                         [] -> tooFewParams f 1
                                         [p] -> do {t <- chk p; return (f', t)}
                                         _ ->  tooManyParams f 1
                  Just (f', Bin chk) -> case xs of
                                          [] -> tooFewParams f 1
                                          [_] -> tooFewParams f 1
                                          [a, b] -> do {t <- chk a b; return (f', t)}
                                          _ -> tooManyParams f 2
                  Nothing -> throwError $ "undefined function: " ++ f

tooManyParams :: Id -> Int -> Either String (UFun, Typ)
tooManyParams f exp = throwError $ "too many parameters: " ++ f ++ " (expected " ++ show exp ++ ")"

tooFewParams :: Id -> Int -> Either String (UFun, Typ)
tooFewParams f exp = throwError $ "too few parameters: " ++ f ++ "(expected " ++ show exp ++ ")"

-- |Stores name, ast node, parameter type, return type of functions.
funtypes :: [(Id, (UFun, UFunTyp))]
funtypes = [("functions", (UFunctions, Un (\t -> do {expect Mod t; return $ List Fun})))
           , ("arity", (UArity, Un (\t -> expectThen Fun t Int)))
           , ("null", (UNull, Un (\t -> case t of List _ -> return Bool; _ -> throwError "type error")))
           , ("calls", (UCalls, Un (\t -> expectThen Fun t (List Fun))))
           , ("path", (UPath, Un (\t -> expectThen File t FilePath)))
           , ("file", (UFile, Un (\t -> expectThen Mod t (List File))))
           , ("exported", (UExported, Un (\t -> expectThen Fun t Bool)))
           , ("recursivity", (URecursivity, Un (\t -> expectThen Fun t FunRecursivity)))
           , ("returns", (UReturns, Un (\t -> expectThen Fun t Type)))
           , ("parameters", (UParameters, Un (\t -> expectThen Fun t (List Expr))))
           , ("not", (UNot, Un (\t -> expectThen Bool t Bool)))
           , ("elem", (UElem, Bin (\a b -> case b of List x -> expectThen a x Bool; _ -> throwError "type error")))
           ]

-- |Checks whether the particular type have name function.
named :: Typ -> Either String ()
named t | t `elem` [File,Mod,Fun,Record] = return ()
        | otherwise = throwError $ "dont have name: " ++ show t

referencable :: Typ -> Either String ()
referencable t | t `elem` [Fun,Record] = return ()
               | otherwise = throwError $ "not referencable :" ++ show t

expectThen :: Typ -> Typ -> Typ -> Either String Typ
expectThen exp act ret = do {expect exp act; return ret}

expect :: Typ -> Typ -> Either String ()
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
                args <- many1 (var <|> app <|> query)
                return (UAppExpr (UFName f) args))

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

atFile :: Parser UQuery
atFile = reserved "atFile" `as` UAtFile

atFunction :: Parser UQuery
atFunction = reserved "atFunction" `as` UAtFunction

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

