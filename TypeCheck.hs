{-# LANGUAGE FlexibleContexts #-}

module TypeCheck where

import Data.Maybe (isJust)
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Error (throwError,catchError)
import Control.Applicative ((<$>))
import Text.Read (readMaybe)
import Types

getType :: Id -> QCheck Typ
getType v = do 
  env <- get
  case lookup v env of
    Just t  -> return t
    Nothing -> throwError ("undefined identifier: " ++ v)
               
setType :: Typ -> Typ -> QCheck ()
setType old new = do
  namespace <- get
  let namespace' = map update namespace
  put namespace'
    where update e@(x, t)
              | t == old  = (x,new)
              | otherwise = e

addVar :: Id -> Typ -> QCheck ()
addVar v t = modify ((v,t):)

type QCheck a = StateT Namespace (WriterT [String] (Either String)) a

runQCheck :: QCheck a -> Namespace -> Either String ((a,Namespace),[String])
runQCheck q ns = runWriterT (runStateT q ns)

evalQCheck :: QCheck a -> Namespace -> Either String (a, [String])
evalQCheck q ns = case runQCheck q ns of
                    Right ((x,_),warns) -> Right (x, warns)
                    Left err            -> Left err
    
type Namespace = [(Id, Typ)]

-- |Type-checks and transforms untyped queries.
check :: UQuery -> QCheck TUQuery
check (UBind m (UF x body)) = do
  m' ::: List tm <- check m
  addVar x tm
  body' ::: List tbody <- check body 
  return $ UBind m' (UF x body') ::: List tbody
check (UReturn x) = do
  x' ::: t <- check x
  return $ UReturn x' ::: List t
check (UTuple xs) = do
  xs' <- mapM check xs
  let (ys,tys) = unzip [(y,ty) | y ::: ty <- xs']
  return $ UTuple ys ::: Tuple tys
check (UGroupBy f q) = do
  q' ::: List tq <- check q
  ft <- getType f
  apptype <- typeCheck f ft [tq]
  return $ UGroupBy f q' ::: Grouped apptype tq
check (UWith defs q) = do
  defs' <- mapM check defs
  let ds = [d | d ::: _ <- defs']
  q' ::: tq <- check q
  return $ UWith ds q' ::: tq
check (UFunDef f args body) = do
  checkIsDefined f
  fundef ::: ftype <- checkFunDef f args body    
  addVar f ftype
  return (fundef ::: ftype)
check q@(URef name) = do
  t <- getType name
  return (q ::: t)
--check (UVarExpr v) = getVar v
check (UDataConst cons) =
    case readMaybe cons of 
      Just x -> return $ UExprTypeLit x ::: ExprType
      Nothing -> do
        case readMaybe cons of
          Just x -> return $ UFunRecurLit x ::: FunRecursivity
          Nothing -> throwError $ "unknown literal: " ++ cons
check UModules = return $ UModules ::: List Mod
check UFiles = return $ UFiles ::: List File
check UAtFile = return $ UAtFile ::: File
check UAtModule = return $ UAtModule ::: Mod
check UAtFunction = return $ UAtFunction ::: Fun
check UAtExpr = return $ UAtExpr ::: Expr
check (UAppExpr f args) = do
  targs' <- mapM check args
  let (args', argtypes') = unzip [(arg, argt) | arg ::: argt <- targs']
  ft <- checkApp f argtypes'
  return $ (UAppExpr f args') ::: ft
check (UFunComp args) = do
  types <- mapM getType args
  let h:t = reverse types
  compType <- foldM step h t
  return $ UFunComp args ::: compType
    where
      step :: Typ -> Typ -> QCheck Typ
      step compType atype = fst <$> compose atype compType []

check (URelation op q1 q2) = do
  q1' ::: t1 <- check q1
  q2' ::: t2 <- check q2
  let relType = relationType op
  _ <- typeCheck (show op) relType [t1,t2]
  return $ (URelation op q1' q2') ::: Bool
check (UGuard p) = do
  p' ::: t <- check p
  expect Bool t
  return $ UGuard p' ::: List Unit
check q@(UNumLit _) = return $ q ::: Int
check q@(UStringLit _) = return $ q ::: String

-- |Checks the argument types.
checkApp :: Id -> [Typ] -> QCheck Typ
checkApp f argTypes = do
  fType <- getType f
  resType <- typeCheck f fType argTypes
  return resType

checkIsDefined :: Id -> QCheck ()
checkIsDefined f = do
  namespace <- get
  when (isJust . lookup f $ namespace)
           (throwError ("function already defined: " ++ f))

checkFunDef :: Id -> [Id] -> UQuery -> QCheck TUQuery
checkFunDef f args body = do 
  namespace <- get
  zipWithM_ addArg args ['a'..]
  body' ::: bodyType <- check body `catchError` handler
  argTypes <- forM args getType
  let ftype = makeFunType argTypes bodyType
  put namespace
  return $ UFunDef f args body' ::: ftype
      where addArg arg c = addVar arg (TV c)
            handler err = throwError (err ++ "\nin function definition " ++ f)

makeFunType :: [Typ] -> Typ -> Typ
makeFunType args bodyt = let (cs, ftype) = foldr step ([],bodyt) args
                         in foldr (:=>:) ftype cs
    where step (c :=>: a) (cs, acc) = (c:cs, a :->: acc)
          step arg (cs,acc)         = (cs, arg :->: acc)

type TEnv = [(Typ,Typ)]

typeCheck :: Id -> Typ -> [Typ] -> QCheck Typ
typeCheck f t args = fst <$> tcheck t args [] 1
    where
      tcheck :: Typ -> [Typ] -> TEnv-> Int -> QCheck (Typ,TEnv)
      tcheck (_ :->: _) [] _env _ind =
          tooFewParams f argsCount (length args)
      tcheck (a :->: b) (x:xs) env ind = do
        env' <- unify a x env ind
        tcheck b xs env' (ind + 1)
      tcheck (constr :=>: a) args env ind = do
        res@(_,env') <- tcheck a args env ind
        checkConst constr env'
        return res
      tcheck _ (_:_) _env _ind = 
          tooManyParams f argsCount (length args)
      tcheck (List a) [] env ind = do
        (resT, _) <- tcheck a [] env ind
        return (List resT, env)
      tcheck a [] env _ind
          | typeVar a = case lookup a env of
                          Just b -> return (b,env)
                          Nothing -> return (a,env)
          | otherwise = return (a,env)

      unify :: Typ -> Typ -> TEnv -> Int -> QCheck TEnv
      unify t1@(a :->: b) t2@(c :->: d) env ind =
          catchError (do
            env' <- unify a c env ind
            unify b d env' ind)
          (\_ -> throwError $ errorMsg t1 t2 ind)
      unify (List a) (List b) env ind = unify a b env ind
      unify a b@(TV v) env ind 
          | typeVar a = do let tv = case lookup a env of
                                      Just ta  -> ta
                                      Nothing  -> argType t ind
                           setType b tv
                           return $ (a,b):env
          | otherwise = do setType b a
                           return env                 
      unify a b env ind
          | typeVar a  = case lookup a env of 
                           Just at -> unify at b env ind
                           Nothing -> do
                             setType a b
                             return $ (a,b):env
          | a == b     = return env
          | otherwise  = throwError $ errorMsg a b ind

      errorMsg e a ind = "type error: expected: " ++ show e ++ " actual: " ++ show a ++ "\nat the " ++ show ind ++ ". argument of " ++ f

      argsCount = fArgs t 0

      fArgs (_ :=>: b) n = fArgs b n
      fArgs (_ :->: b) n = fArgs b (n + 1)
      fArgs _          n = n

argType :: Typ -> Int -> Typ
argType (a :->: _) 1 = a
argType (_ :->: b) n = argType b (n - 1)
argType (Named a :=>: b) n = if t == a then Named a :=>: a else t
    where t = argType b n
argType (Referencable a :=>: b) n = if t == a then Referencable a :=>: a else t
    where t = argType b n
argType (Typeable a :=>: b) n = if t == a then Typeable a :=>: a else t
    where t = argType b n
argType (MultiLine a :=>: b) n = if t == a then MultiLine a :=>: a else t
    where t = argType b n
argType (MultiExpression a :=>: b) n = if t == a then MultiExpression a :=>: a else t
    where t = argType b n
argType (Ord a :=>: b) n = if t == a then Ord a :=>: a else t
    where t = argType b n
argType a _ = error ("argType: not function type: " ++ show a)

tooManyParams :: Id -> Int -> Int -> QCheck a
tooManyParams f expected actual = throwError $ "too many parameters: " ++ f ++ " (expected " ++ show expected ++ ", actual: " ++ show actual ++ ")" 

tooFewParams :: Id -> Int -> Int -> QCheck a
tooFewParams f expected actual = throwError $ "too few parameters: " ++ f ++ " (expected " ++ show expected ++ ", actual: " ++ show actual ++ ")"

type ErrMsg = String

-- |Associates function name with type.
funtypes :: [(Id, Typ)]
funtypes = 
    [ ("functions", Mod :->: List Fun)
    , ("name", Named a :=>: a :->: String)
    , ("arity", Fun :->: Int)
    , ("loc", MultiLine a :=>: a :->: List Int)
    , ("null", List a :->: Bool)
    , ("calls", Fun :->: List Fun)
    , ("path",  File :->: FilePath)
    , ("directory", File :->: FilePath)
    , ("filename", File :->: FilePath)
    , ("file",  Mod :->: List File)
    , ("module", File :->: List Mod)
    , ("defmodule", Fun :->: Mod)
    , ("records",  File :->: List Record)
    , ("exported", Fun :->: Bool)
    , ("recursivity",  Fun :->: FunRecursivity)
    , ("references", Referencable a :=>: a :->: List Expr)
    , ("returns", Fun :->: List Type)
    , ("parameters",Fun :->: List FunParam)
    , ("type",  Typeable a :=>: a :->: Type)
    , ("exprType", Expr :->: ExprType)
    , ("expressions", MultiExpression a :=>: a :->: List Expr)
    , ("not", Bool :->: Bool)
    , ("∪", List a :->: List a :->: List a)
    , ("∈", a :->: List a :->: Bool)
    , ("⊆", List a :->: List a :->: Bool)
    , ("any_in", List a :->: List a :->: Bool)
    , ("origin", Expr :->: List Expr)
    , ("reach", Expr :->: List Expr)
    , ("fields", Record :->: List RecordField)
    , ("closureN", Int :->: (a :->: List a) :->: a :->: List a)
    , ("lfp", (a :->: List a) :->: a :->: List a)
    , ("iteration", Int :->: (a :->: List a) :->: a :->: List a)
    , ("chainN", Int :->: (a :->: List a) :->: a :->: List (Chain a))
    , ("chainInf", (a :->: List a) :->: a :->: List (Chain a))
    , ("max", Ord a :=>: List a :->: List a)
    , ("min", Ord a :=>: List a :->: List a)
    , ("average", List Int :->: List Int)
    , ("count", Chain a :->: Int)
    , ("distinct", Chain a :->: Chain a)
    ]
    where a = TV 'a'

relationType :: Binop -> Typ
relationType Regexp = String :->: String :->: Bool
relationType _      = a :->: a :->: Bool
    where a = TV 'a'

-- |Decides whether the particular type have name function.
named :: Typ -> QCheck ()
named (TV _)  = return ()
named t | t `elem` [File,Mod,Fun,Record,RecordField] = return ()
        | otherwise = throwError $ "doesn't have name: " ++ show t

-- |Decides whether the particular type is referencable.
referencable :: Typ -> QCheck ()
referencable (TV _) = return ()
referencable t | t `elem` [Fun,Record,RecordField] = return ()
               | otherwise = throwError $ "not referencable: " ++ show t
                             
typeable :: Typ -> QCheck ()
typeable (TV _) = return ()
typeable t | t `elem` [FunParam,RecordField] = return ()
           | otherwise = throwError $ "not typeable: " ++ show t

multiline :: Typ -> QCheck ()
multiline (TV _) = return ()
multiline t | t `elem` [File,Mod,Fun] = return ()
            | otherwise = throwError $ "can't count line of codes: " ++ show t

multiexpr :: Typ -> QCheck ()
multiexpr (TV _) = return ()
multiexpr t | t `elem` [Fun,Expr] = return ()
            | otherwise = throwError $ "doesn't have expressions: " ++ show t

ord :: Typ -> QCheck ()
ord (TV _) = return ()
ord t | t `elem` [Int,String,Bool] = return ()
      | otherwise = throwError $ "can't be ordered: " ++ show t

type TypEnv = [(Typ,Typ)]

checkConst :: TypConstraint -> TypEnv -> QCheck ()
checkConst constr env = case constr of
                         Named a           -> getTyp a >>= named
                         Referencable a    -> getTyp a >>= referencable
                         Typeable a        -> getTyp a >>= typeable
                         MultiLine a       -> getTyp a >>= multiline
                         MultiExpression a -> getTyp a >>= multiexpr
                         Ord a             -> getTyp a >>= ord
    where getTyp a = case lookup a env of
                       Just t  -> return t
                       Nothing -> throwError ("constraint error: " ++ show a)

compose :: Typ -> Typ -> TypEnv ->  QCheck (Typ,[(Typ,Typ)])
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
{-
argType :: Typ -> Typ
argType (c :=>: b) = c :=>: argType b
argType (a :->: b) | funType b = a :->: argType b
                   | otherwise = a
-}
isFunType :: Typ -> Bool
isFunType (_ :=>: b) = isFunType b
isFunType (_ :->: _) = True
isFunType _ = False

typeVar :: Typ -> Bool
typeVar (TV _) = True
typeVar _      = False

expect :: Typ -> Typ -> QCheck ()
expect (List a) (List b) = expect a b
expect expected actual 
    | typeVar expected = return ()
    | actual == expected = return ()
    | otherwise = throwError $ "type error: expected: " ++ show expected ++ ", actual: " ++ show actual
