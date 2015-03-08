module TypeCheck where

import Control.Monad.Error (throwError)
import Control.Monad (void,foldM)
import Control.Applicative ((<$>))
import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Types

runchk :: String -> Parser UQuery -> TEnv ->  Either String TUQuery
runchk s parser env = case parse parser "" s of
                        Right x -> check x env
                        Left err -> throwError . show $ err

getVar :: TEnv -> Id -> Either String Typ
getVar env v = case lookup v env of
                 Just x  -> return x
                 Nothing -> throwError $ "unbound variable: " ++ v

type TEnv = [(Id,Typ)]

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
check (UFunComp args) env = do
  (args', types) <- unzip <$> mapM (funtype . fname) args
  let h:t = reverse types
  compType <- foldM step h t
  return $ UFunComp args' ::: compType
    where
      step :: Typ -> Typ -> Either ErrMsg Typ
      step compType atype = fst <$> compose atype compType []

      fname (UFName f) = f

check (URelation op q1 q2) env = do
  q1' ::: t1 <- check q1 env
  q2' ::: t2 <- check q2 env
  let relType = relationType op
  _ <- typeCheck (show op) relType [t1,t2]
  return $ (URelation op q1' q2') ::: Bool
check (UGuard p) env = do
  p' ::: t <- check p env
  expect Bool t
  return $ UGuard p' ::: List Unit
check q@(UNumLit _) _env = return $ q ::: Int
check q@(UStringLit _) _env = return $ q ::: String

-- |Maps function name to tree node, and checks the argument types.
checkFun :: Id -> [Typ] -> Either String (UFun, Typ)
checkFun f argTypes = do
  (f', fType) <- funtype f
  resType <- typeCheck f fType argTypes
  return (f', resType)

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

      unify (a :->: b) (c :->: d) env = do 
        env' <- unify a c env
        unify b d env'
      unify (List a) (List b) env = unify a b env
      unify a b  env | typeVar a  = case lookup a env of 
                                      Just t -> unify t b env
                                      Nothing ->  return $ (a,b):env
                     | a == b     = return env
                     | otherwise  = throwError $ "type error: expected: " ++ show a ++ " actual: " ++ show b

      countArgs t = tArgs t 0

      tArgs (_ :=>: b) n = tArgs b n
      tArgs (_ :->: b) n = tArgs b (n + 1)
      tArgs _          n = n

tooManyParams :: Id -> Int -> Int -> Either String a
tooManyParams f expected actual = throwError $ "too many parameters: " ++ f ++ " (expected " ++ show expected ++ ", actual: " ++ show actual ++ ")" 

tooFewParams :: Id -> Int -> Int -> Either String a
tooFewParams f expected actual = throwError $ "too few parameters: " ++ f ++ " (expected " ++ show expected ++ ", actual: " ++ show actual ++ ")"

type ErrMsg = String

-- |Associates function name with ast node and function which checks argument types.
funtype :: Id -> Either ErrMsg (UFun, Typ)
funtype "functions"   = return $ (UFunctions, Mod :->: List Fun)
funtype "name"        = return $ (UName, Named A :=>: A :->: String)
funtype "arity"       = return $ (UArity, Fun :->: Int)
funtype "null"        = return $ (UNull, List A :->: Bool)
funtype "calls"       = return $ (UCalls, Fun :->: List Fun)
funtype "path"        = return $ (UPath, File :->: FilePath)
funtype "directory"   = return $ (UDir, File :->: FilePath)
funtype "filename"    = return $ (UFileName, File :->: FilePath)
funtype "file"        = return $ (UFile, Mod :->: List File)
funtype "exported"    = return $ (UExported, Fun :->: Bool)
funtype "recursivity" = return $ (URecursivity, Fun :->: FunRecursivity)
funtype "references"  = return $ (UReferences, Referencable A :=>: A :->: List Expr)
funtype "returns"     = return $ (UReturns, Fun :->: List Type)
funtype "parameters"  = return $ (UParameters, Fun :->: List FunParam)
funtype "type"        = return $ (UTypeOf, Typeable A :=>: A :->: Type)
funtype "exprType"    = return $ (UExprType, Expr :->: ExprType)
funtype "not"         = return $ (UNot, Bool :->: Bool)
funtype "∪"           = return $ (UUnion, List A :->: List A :->: List A)
funtype "∈"           = return $ (UElem, A :->: List A :->: Bool)
funtype "⊆"           = return $ (USubset, List A :->: List A :->: Bool)
funtype "any_in"      = return $ (UAnyIn, List A :->: List A :->: Bool)
funtype "origin"      = return $ (UOrigin, Expr :->: List Expr)
funtype "reach"       = return $ (UReach, Expr :->: List Expr)
funtype "closureN"    = return $ (UClosureN, (A :->: List A) :->: Int :->: List A)
funtype "lfp"         = return $ (ULfp, (A :->: List A) :->: List A)
funtype f             = throwError $ "unknown function: " ++ f

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

type TypEnv = [(Typ,Typ)]

checkConst :: TypConstraint -> TypEnv -> Either ErrMsg ()
checkConst const env = case const of
                         Named a        -> getTyp a >>= named
                         Referencable a -> getTyp a >>= referencable
                         Typeable a     -> getTyp a >>= typeable
    where getTyp a = case lookup a env of
                       Just t  -> return t
                       Nothing -> throwError "constraint error"

compose :: Typ -> Typ -> TypEnv ->  Either ErrMsg (Typ,[(Typ,Typ)])
compose (const :=>: f) g@(a :->: b) env = do
  (t, env') <- compose f g env
  checkConst const env'
  return (t, env')
compose (c :->: d) (a :->: b) env 
    | typeVar c = do  expect (List c) b
                      let (List t) = b
                      return $ (a :->: d, (c,t):env)
                             
    | otherwise = do  expect (List c) b
                      return $ (a :->: d, env)
              
{-
resultType :: Typ -> Typ
resultType (_ :=>: b) = resultType b
resultType (_ :->: b) = resultType b
resultType b = b
-}
argType :: Typ -> Typ
argType (c :=>: b) = c :=>: argType b
argType (a :->: b) | funType b = a :->: argType b
                   | otherwise = a

funType :: Typ -> Bool
funType (_ :=>: b) = funType b
funType (_ :->: _) = True
funType _ = False

typeVar :: Typ -> Bool
typeVar v = v == A || v == B

expect :: Typ -> Typ -> Either String ()
expect (List a) (List b) = expect a b
expect expected actual 
    | typeVar expected = return ()
    | actual == expected = return ()
    | otherwise = throwError $ "type error: expected: " ++ show expected ++ ", actual: " ++ show actual
