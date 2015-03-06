{-# LANGUAGE GADTs #-}
module Parser where

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Language as L
import Control.Applicative ((<*), (*>))
import qualified Sq (DbModule, DbFunction, Named, FileType)
import Control.Monad.Error (throwError)
import Control.Monad (void)
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
    | UExprType
    | UExpressions
      deriving (Show, Eq)

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
    | A     -- ^ Type variable.
    | B     -- ^ Type variable.
    | Typ :->: Typ  -- ^ Function type.
    | TypConstraint :=>: Typ -- ^ Type constraint.
      deriving (Show,Eq)

infixr 4 :->:
infixr 3 :=>:

data TypConstraint 
    = Named Typ
    | Referencable Typ
    | Typeable Typ
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
  let relType = relationType op
  typeCheck (show op) relType [t1,t2]
  return $ (URelation op q1' q2') ::: Bool
check (UGuard p) env = do
  p' ::: t <- check p env
  expect Bool t
  return $ UGuard p' ::: List Unit
check q@(UNumLit _) _env = return $ q ::: Int
check q@(UStringLit _) _env = return $ q ::: String

-- |Maps function name to tree node, and checks the argument types.
checkFun :: Id -> [Typ] -> Either String (UFun, Typ)
checkFun f xs = case funtype f of
                  Just (f', fType) -> do resType <- typeCheck f fType xs 
                                         return (f', resType)
                  Nothing -> throwError $ "unknown function: " ++ f

typeCheck :: Id -> Typ -> [Typ] -> Either String Typ
typeCheck f t args = fst <$> tcheck t args []
    where 
      tcheck (_ :->: _) [] _env = tooFewParams f (countArgs t) (length args)
      tcheck (a :->: b) (x:xs) env = do env' <- unify a x env
                                        tcheck b xs env'
      tcheck (const :=>: a) args env = do res@(_,env') <- tcheck a args env
                                          checkConst const env'
                                          return res
      tcheck _ (_:_) env = tooManyParams f (countArgs t) (length args)
      tcheck (List a) [] env = do (resT, _) <- tcheck a [] env
                                  return (List resT, env)
      tcheck a [] env = case lookup a env of
                          Just b -> return (b,env)
                          Nothing -> return (a,env)

      unify (List a) (List b) env = unify a b env
      unify a b  env | typeVar a  = case lookup a env of 
                                      Just t -> unify t b env
                                      Nothing ->  return $ (a,b):env
                     | a == b     = return env
                     | otherwise  = throwError $ "type error: expected: " ++ show a ++ " actual: " ++ show b

      typeVar v = v == A || v == B

      checkConst (Named a)        env = case lookup a env of
                                          Just t -> named t >> return env
                                          Nothing -> throwError "constraint error"
      checkConst (Referencable a) env = case lookup a env of
                                          Just t -> referencable t >> return env
                                          Nothing -> throwError "constraint error"
      checkConst (Typeable a) env     = case lookup a env of
                                          Just t -> typeable t >> return env
                                          Nothing -> throwError "constraint error"

      countArgs t = tArgs t 0

      tArgs (_ :=>: b) n = tArgs b n
      tArgs (_ :->: b) n = tArgs b (n + 1)
      tArgs _          n = n

tooManyParams :: Id -> Int -> Int -> Either String a
tooManyParams f expected actual = throwError $ "too many parameters: " ++ f ++ " (expected " ++ show expected ++ ", actual: " ++ show actual ++ ")" 

tooFewParams :: Id -> Int -> Int -> Either String a
tooFewParams f expected actual = throwError $ "too few parameters: " ++ f ++ " (expected " ++ show expected ++ ", actual: " ++ show actual ++ ")"

-- |Associates function name with ast node and function which checks argument types.
funtype :: Id -> Maybe (UFun, Typ)
funtype "functions"   = Just (UFunctions, Mod :->: List Fun)
funtype "name"        = Just (UName, Named A :=>: A :->: String)
funtype "arity"       = Just (UArity, Fun :->: Int)
funtype "null"        = Just (UNull, List A :->: Bool)
funtype "calls"       = Just (UCalls, Fun :->: List Fun)
funtype "path"        = Just (UPath, File :->: FilePath)
funtype "directory"   = Just (UDir, File :->: FilePath)
funtype "filename"    = Just (UFileName, File :->: FilePath)
funtype "file"        = Just (UFile, Mod :->: List File)
funtype "exported"    = Just (UExported, Fun :->: Bool)
funtype "recursivity" = Just (URecursivity, Fun :->: FunRecursivity)
funtype "references"  = Just (UReferences, Referencable A :=>: A :->: List Expr)
funtype "returns"     = Just (UReturns, Fun :->: List Type)
funtype "parameters"  = Just (UParameters, Fun :->: List FunParam)
funtype "type"        = Just (UTypeOf, Typeable A :=>: A :->: Type)
funtype "exprType"    = Just (UExprType, Expr :->: ExprType)
funtype "not"         = Just (UNot, Bool :->: Bool)
funtype "∪"           = Just (UUnion, List A :->: List A :->: List A)
funtype "∈"           = Just (UElem, A :->: List A :->: Bool)
funtype "⊆"           = Just (USubset, List A :->: List A :->: Bool)
funtype "any_in"      = Just (UAnyIn, List A :->: List A :->: Bool)
funtype "origin"      = Just (UOrigin, Expr :->: List Expr)
funtype "reach"       = Just (UReach, Expr :->: List Expr)
funtype _             = Nothing

relationType :: Binop -> Typ
relationType Regexp = String :->: String :->: Bool
relationType _      = A :->: A :->: Bool

-- |Decides whether the particular type have name function.
named :: Typ -> Either String ()
named t | t `elem` [File,Mod,Fun,Record] = return ()
        | otherwise = throwError $ "dont have name: " ++ show t

-- |Decides whether the particular type is referencable.
referencable :: Typ -> Either String ()
referencable t | t `elem` [Fun,Record] = return ()
               | otherwise = throwError $ "not referencable: " ++ show t
                             
typeable :: Typ -> Either String ()
typeable t | t `elem` [FunParam, RecordField] = return ()
           | otherwise = throwError $ "not typeable: " ++ show t

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
        { T.opStart = oneOf "<=>∪⊆∈"
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
  as <- try $ (query <|> initial <|> var <|> app) <* reservedOp op
  bs <- query <|> initial <|> app
  return $ UAppExpr (UFName op) [as,bs]

union :: Parser UQuery
union = infixSetOp "∪" <?> "union"

subset :: Parser UQuery
subset = infixSetOp "⊆" <?> "subset of"

element :: Parser UQuery
element = infixSetOp "∈" <?> "element of"

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

