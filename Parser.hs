{-# LANGUAGE GADTs #-}
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
import Data.Maybe (fromJust)

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
    | UAtModule 
    | UAtFunction
    | UAtExpr
      deriving Show

-- |Applicable functions of the query language.
data UFun
    = UFunctions -- ^ Functions of a module.
    | UPath      -- ^ Path of a loaded file.
    | UDir       -- ^ Directory containing a file.
    | UFileName  -- ^ Name of a loaded file.
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
    | UUnion     -- ^ Data.List.union
    | USubset
    | UAnyIn
    | UFName String -- ^ Function identified by its name.
    | UExported
    | URecursivity
    | UReturns
    | UReferences
    | UParameters
    | UOrigin
    | UReach
    | UTypeOf
    | UExpressions
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
    | RecordField
    | Spec
    | SpecParam
    | FunParam
    | Type    -- ^ Sq.DbType
    | ExprType
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

-- |Type-checks and transforms untyped queries.
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
check UAtModule _env = return $ UAtModule ::: Mod
check UAtFunction _env = return $ UAtFunction ::: Fun
check UAtExpr _env = return $ UAtExpr ::: Expr
check (UAppExpr (UFName f) args) env = do
  targs' <- mapM (flip check env) args
  let (args', argtypes') = unzip [(arg, argt) | arg ::: argt <- targs']
  (f', ft) <- checkFun f argtypes'
  return $ (UAppExpr f' args') ::: ft
check (URelation op q1 q2) env = do
  q1' ::: t1 <- check q1 env
  q2' ::: t2 <- check q2 env
  checkRel op t1 t2
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

-- |Maps function name to tree node, and checks the argument types.
checkFun :: Id -> [Typ] -> Either String (UFun, Typ)
checkFun f xs = case funtype f of
                  Just (f', Un chk) -> case xs of
                                         [] -> tooFewParams f 1
                                         [p] -> do {t <- chk p; return (f', t)}
                                         _ ->  tooManyParams f 1
                  Just (f', Bin chk) -> case xs of
                                          [a, b]  -> do {t <- chk a b; return (f', t)}
                                          _:_:_:_ -> tooManyParams f 2
                                          _ -> tooFewParams f 1
                  Nothing -> throwError $ "unknown function: " ++ f

tooManyParams :: Id -> Int -> Either String (UFun, Typ)
tooManyParams f expected = throwError $ "too many parameters: " ++ f ++ " (expected " ++ show expected ++ ")"

tooFewParams :: Id -> Int -> Either String (UFun, Typ)
tooFewParams f expected = throwError $ "too few parameters: " ++ f ++ "(expected " ++ show expected ++ ")"

-- |Associates function name with ast node and function which checks argument types.
funtype :: Id -> Maybe (UFun, UFunTyp)
funtype "functions" = Just (UFunctions, Un tcheck)
    where tcheck t = expectThen Mod t (List Fun)
funtype "name" = Just (UName, Un tcheck)
    where tcheck t = do named t
                        return String
funtype "arity" = Just (UArity, Un tcheck)
    where tcheck t = expectThen Fun t Int
funtype "null" = Just (UNull, Un tcheck)
    where tcheck t = case t of
                       List _ -> return Bool
                       _ -> throwError "type error"
funtype "calls" = Just (UCalls, Un (\t -> expectThen Fun t (List Fun)))
funtype "path" = Just (UPath, Un (\t -> expectThen File t FilePath))
funtype "directory" = Just (UDir, Un (\t -> expectThen File t FilePath))
funtype "filename" = Just (UFileName, Un (\t -> expectThen File t FilePath))
funtype "file" = Just (UFile, Un (\t -> expectThen Mod t (List File)))
funtype "exported" = Just (UExported, Un (\t -> expectThen Fun t Bool))
funtype "recursivity" = Just (URecursivity, Un (\t -> expectThen Fun t FunRecursivity))
funtype "references" = Just (UReferences, Un tcheck)
    where tcheck t = do referencable t 
                        return (List Expr)
funtype "returns" = Just (UReturns, Un (\t -> expectThen Fun t Type))
funtype "parameters" = Just (UParameters, Un (\t -> expectThen Fun t (List Expr)))
funtype "type" = Just (UTypeOf, Un tcheck)
    where tcheck t = do typeable t 
                        return (typeOfTypefun t)
funtype "not" = Just (UNot, Un (\t -> expectThen Bool t Bool))
funtype "∪"   = Just (UUnion, Bin tcheck)
    where tcheck (List a) (List b) = expectThen a b (List a)
          tcheck _  _ = throwError "type error"
funtype "elem" = Just (UElem, Bin tcheck)
    where tcheck a (List x) = expectThen a x Bool
          tcheck _ _        = throwError "type error"
funtype "⊆" = Just (USubset, Bin tcheck)
    where tcheck a@(List _) b@(List _) = expectThen a b Bool; 
          tcheck _ _                   = throwError "type error"
funtype "any_in" = Just (UAnyIn, Bin tcheck)
    where tcheck a@(List _) b@(List _) = expectThen a b Bool; 
          tcheck _ _                   = throwError "type error"
funtype "origin" = Just (UOrigin, Un (\t -> expectThen Expr t (List Expr)))
funtype "reach" = Just (UReach, Un (\t -> expectThen Expr t (List Expr)))
funtype _  = Nothing

checkRel :: Binop -> Typ -> Typ -> Either String ()
checkRel op a b = do
  let typeCheck = fromJust . lookup op $ relations
  typeCheck a b

relations :: [(Binop, Typ -> Typ -> Either String ())]
relations = [ (Eq, expect) 
            , (NEq, expect)
            , (Lt, expect)
            , (Lte, expect)
            , (Gt, expect)
            , (Gte, expect)
            , (Regexp, \a b -> do {expect String a; expect String b})
            ]

-- |Decides whether the particular type have name function.
named :: Typ -> Either String ()
named t | t `elem` [File,Mod,Fun,Record] = return ()
        | otherwise = throwError $ "dont have name: " ++ show t

-- |Decides whether the particular type is referencable.
referencable :: Typ -> Either String ()
referencable t | t `elem` [Fun,Record] = return ()
               | otherwise = throwError $ "not referencable: " ++ show t

typeable :: Typ -> Either String ()
typeable t | t `elem` [FunParam, Expr,RecordField] = return ()
           | otherwise = throwError $ "not typeable: " ++ show t

typeOfTypefun :: Typ -> Typ
typeOfTypefun RecordField = Type
typeOfTypefun Expr        = ExprType
typeOfTypefun FunParam    = Type

-- | Decides whether actual argument type equals to expected type. If it does, returns the third argument.
expectThen :: Typ -> Typ -> Typ -> Either String Typ
expectThen expected actual returnType = do {expect expected actual; return returnType}

-- | Decides whether actual argument type equals to expected type. If it does not, throws type error.
expect :: Typ -> Typ -> Either String ()
expect expected actual 
    | actual == expected = return ()
    | otherwise = throwError $ "type error: expected: " ++ show expected ++ ", actual: " ++ show actual

data Binop
    = Eq
    | NEq
    | Lt
    | Lte
    | Gt
    | Gte
    | Regexp
      deriving (Show,Eq)

--- Parsers:

sqDef = L.haskellStyle
        { T.opStart = oneOf "<=>∪⊆"
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

var :: Parser UQuery
var = UVarExpr <$> identifier <?> "variable"

app :: Parser UQuery
app = parens app
      <|>
      try (do f <- identifier
              args <- many1 argument
              return (UAppExpr (UFName f) args))
      <?> "function application"
          where argument = initial <|> var <|> relation <|> app <|> query

infixSetOp :: String -> Parser UQuery
infixSetOp op = do
  as <- try $ (query <|> app) <* reservedOp op
  bs <- query <|> app
  return $ UAppExpr (UFName op) [as,bs]

union :: Parser UQuery
union = infixSetOp "∪" <?> "union"

subset :: Parser UQuery
subset = infixSetOp "⊆" <?> "subset of"

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
bindable = initial <|> app <|> union <|> query

following :: Parser UQuery
following = (comma *> (filter <|> bind)) <|> (vline *> ret)

filter :: Parser UQuery
filter = do
  f <- relation <|> subset <|> app
  rest <- following
  return (UBind (UGuard f) (UF "()" rest))

vline :: Parser String
vline = symbol "|"

ret :: Parser UQuery
ret = UReturn <$> (app <|> var <|> query)

relation :: Parser UQuery
relation = parens relation <|>
           do rel <- try $ do 
                       a1 <- relOperand
                       rel <- relop
                       return $ URelation rel a1
              a2 <- relOperand
              return $ rel a2
           <?> "relation"

relOperand :: Parser UQuery
relOperand = app <|> var <|> numLit <|> stringLit

stringLit :: Parser UQuery
stringLit = UStringLit <$> stringLiteral

numLit :: Parser UQuery
numLit = do 
  n <- lexeme decimal
  return $ UNumLit (fromIntegral n)

relop :: Parser Binop
relop = (eq <|> neq <|> lte <|> lt <|> gte <|> gt <|> regexp) <* spaces

eq :: Parser Binop
eq = try $ symbol "==" `as` Eq

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

regexp :: Parser Binop
regexp = symbol "=~" `as` Regexp

initial :: Parser UQuery
initial = modules <|> atModule <|> atFile <|> atExpression <?> "initial selector"

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

