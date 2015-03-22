{-# LANGUAGE FlexibleContexts #-}

module TypeCheck where

import Data.Maybe (isJust)
import Control.Monad.State
import Control.Monad.Error (throwError,catchError)
import Control.Applicative ((<$>))
import Text.Read (readMaybe)
import Types

getVar :: Id -> QCheck TUQuery
getVar v = do 
  env <- get
  case lookup v env of
    Just x  -> return x
    Nothing -> throwError ("undefined variable: " ++ v)

addVar :: Id -> TUQuery -> QCheck ()
addVar v expr = modify ((v,expr):)

type QCheck a = StateT Namespace (Either String) a

runQCheck :: QCheck a -> Namespace -> Either String (a,Namespace)
runQCheck q ns = runStateT q ns
    
type Namespace = [(Id, TUQuery)]

-- |Type-checks and transforms untyped queries.
check :: UQuery -> QCheck TUQuery
check (UBind m (UF x body)) = do
  m' ::: List tm <- check m
  addVar x (UVarExpr x ::: tm)
  body' ::: List tbody <- check body 
  return $ UBind m' (UF x body') ::: List tbody
check (UReturn x) = do
  x' ::: t <- check x
  return $ UReturn x' ::: List t
check (UTuple xs) = do
  xs' <- mapM check xs
  let (ys,tys) = unzip [(y,ty) | y ::: ty <- xs']
  return $ UTuple ys ::: Tuple tys
check (UGroupBy (UFName f) q) = do
  q' ::: List tq <- check q
  (f',ft) <- getFunType f
  apptype <- typeCheck f ft [tq]
  return $ UGroupBy f' q' ::: Grouped apptype tq
check (UWith defs q) = do
  mapM_ check defs
  q' ::: tq <- check q
  return $ q' ::: tq
check (UFunDef f args body) = do
  checkIsDefined f
  ftype <- checkFunDef f args body    
  addVar f ftype
  return ftype
check (URef name) | knownFun name = 
                      do (f,t) <- getFunType name
                         return $ UFunRef f ::: t
                  | otherwise = getVar name
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
check (UAppExpr (UFName f) args) = do
  targs' <- mapM check args
  let (args', argtypes') = unzip [(arg, argt) | arg ::: argt <- targs']
  (f', ft) <- checkApp f argtypes'
  return $ (UAppExpr f' args') ::: ft
check (UFunComp args) = do
  (args', types) <- unzip <$> mapM (getFunType . fname) args
  let h:t = reverse types
  compType <- foldM step h t
  return $ UFunComp args' ::: compType
    where
      step :: Typ -> Typ -> QCheck Typ
      step compType atype = fst <$> compose atype compType []

      fname (UFName f) = f

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

-- |Maps function name to tree node, and checks the argument types.
checkApp :: Id -> [Typ] -> QCheck (UFun, Typ)
checkApp f argTypes = do
  (f', fType) <- getFunType f
  resType <- typeCheck f fType argTypes
  return (f', resType)

checkIsDefined :: Id -> QCheck ()
checkIsDefined f = do
  namespace <- get
  when (isJust . lookup f $ namespace) (throwError ("function already defined: " ++ f))

checkFunDef :: Id -> [Id] -> UQuery -> QCheck TUQuery
checkFunDef f args body = do 
  namespace <- get
  forM_ args (\v -> addVar v (UVarExpr v ::: Infer))
  body' ::: ftype <- check body  
  put namespace
  return $ UFunDef f args body' ::: ftype

typeCheck :: Id -> Typ -> [Typ] -> QCheck Typ
typeCheck f t args = fst <$> tcheck t args [] 1
    where 
      tcheck (_ :->: _) [] _env _ind = tooFewParams f (countArgs t) (length args)
      tcheck (a :->: b) (x:xs) env ind = do env' <- unify a x env ind
                                            tcheck b xs env' (ind + 1)
      tcheck (const :=>: a) args env ind = do res@(_,env') <- tcheck a args env ind
                                              checkConst const env'
                                              return res
      tcheck _ (_:_) env _ind = tooManyParams f (countArgs t) (length args)
      tcheck (List a) [] env ind = do (resT, _) <- tcheck a [] env ind
                                      return (List resT, env)
      tcheck a [] env _ind | typeVar a = case lookup a env of
                                           Just b -> return (b,env)
                                           Nothing -> return (a,env)
                           | otherwise = return (a,env)

      unify t1@(a :->: b) t2@(c :->: d) env ind =
          catchError (do
            env' <- unify a c env ind
            unify b d env' ind)
          (\_ -> throwError $ errorMsg t1 t2 ind)
      unify (List a) (List b) env ind = unify a b env ind
      unify a b  env ind | typeVar a  = case lookup a env of 
                                          Just t -> unify t b env ind
                                          Nothing ->  return $ (a,b):env
                         | a == b     = return env
                         | otherwise  = throwError $ errorMsg a b ind

      errorMsg e a ind = "type error: expected: " ++ show e ++ " actual: " ++ show a ++ "\nat the " ++ show ind ++ ". argument of " ++ f

      countArgs t = fArgs t 0

      fArgs (_ :=>: b) n = fArgs b n
      fArgs (_ :->: b) n = fArgs b (n + 1)
      fArgs _          n = n

tooManyParams :: Id -> Int -> Int -> QCheck a
tooManyParams f expected actual = throwError $ "too many parameters: " ++ f ++ " (expected " ++ show expected ++ ", actual: " ++ show actual ++ ")" 

tooFewParams :: Id -> Int -> Int -> QCheck a
tooFewParams f expected actual = throwError $ "too few parameters: " ++ f ++ " (expected " ++ show expected ++ ", actual: " ++ show actual ++ ")"

type ErrMsg = String

knownFun :: Id -> Bool
knownFun name = isJust $ lookup name funtypes

getFunType :: Id -> QCheck (UFun, Typ)
getFunType f = case lookup f funtypes of
                 Just t  -> return t
                 Nothing -> throwError $ "unknown function: " ++ f

-- |Associates function name with ast node and function which checks argument types.
funtypes :: [(Id, (UFun, Typ))]
funtypes = 
    [ ("functions", (UFunctions, Mod :->: List Fun))
    , ("name", (UName, Named A :=>: A :->: String))
    , ("arity", (UArity, Fun :->: Int))
    , ("loc", (ULoc, MultiLine A :=>: A :->: List Int))
    , ("null", (UNull, List A :->: Bool))
    , ("calls", (UCalls, Fun :->: List Fun))
    , ("path",  (UPath, File :->: FilePath))
    , ("directory",  (UDir, File :->: FilePath))
    , ("filename",  (UFileName, File :->: FilePath))
    , ("file",  (UFile, Mod :->: List File))
    , ("module", (UModule, File :->: List Mod))
    , ("defmodule", (UDefModule, Fun :->: Mod))
    , ("records",  (URecords, File :->: List Record))
    , ("exported",  (UExported, Fun :->: Bool))
    , ("recursivity",  (URecursivity, Fun :->: FunRecursivity))
    , ("references",  (UReferences, Referencable A :=>: A :->: List Expr))
    , ("returns",  (UReturns, Fun :->: List Type))
    , ("parameters",  (UParameters, Fun :->: List FunParam))
    , ("type",  (UTypeOf, Typeable A :=>: A :->: Type))
    , ("exprType",  (UExprType, Expr :->: ExprType))
    , ("expressions",  (UExpressions, MultiExpression A :=>: A :->: List Expr))
    , ("not",  (UNot, Bool :->: Bool))
    , ("∪", (UUnion, List A :->: List A :->: List A))
    , ("∈", (UElem, A :->: List A :->: Bool))
    , ("⊆", (USubset, List A :->: List A :->: Bool))
    , ("any_in",  (UAnyIn, List A :->: List A :->: Bool))
    , ("origin",  (UOrigin, Expr :->: List Expr))
    , ("reach",  (UReach, Expr :->: List Expr))
    , ("fields",  (UFields, Record :->: List RecordField))
    , ("closureN",  (UClosureN, Int :->: (A :->: List A) :->: A :->: List A))
    , ("lfp",  (ULfp, (A :->: List A) :->: A :->: List A))
    , ("iteration",  (UIteration, Int :->: (A :->: List A) :->: A :->: List A))
    , ("chainN",  (UChainN, Int :->: (A :->: List A) :->: A :->: List (Chain A)))
    , ("chainInf",  (UChainInf, (A :->: List A) :->: A :->: List (Chain A)) )
    , ("max",  (UMax, Ord A :=>: List A :->: List A))
    , ("min",  (UMin, Ord A :=>: List A :->: List A))
    , ("average",  (UAverage, List Int :->: List Int))
    , ("count", (ULength, Chain A :->: Int))
    , ("distinct", (UDistinct, Chain A :->: Chain A))
    ]

relationType :: Binop -> Typ
relationType Regexp = String :->: String :->: Bool
relationType _      = A :->: A :->: Bool

-- |Decides whether the particular type have name function.
named :: Typ -> QCheck ()
named t | t `elem` [File,Mod,Fun,Record,RecordField] = return ()
        | otherwise = throwError $ "doesn't have name: " ++ show t

-- |Decides whether the particular type is referencable.
referencable :: Typ -> QCheck ()
referencable t | t `elem` [Fun,Record,RecordField] = return ()
               | otherwise = throwError $ "not referencable: " ++ show t
                             
typeable :: Typ -> QCheck ()
typeable t | t `elem` [FunParam,RecordField] = return ()
           | otherwise = throwError $ "not typeable: " ++ show t

multiline :: Typ -> QCheck ()
multiline t | t `elem` [File,Mod,Fun] = return ()
            | otherwise = throwError $ "can't count line of codes: " ++ show t

multiexpr :: Typ -> QCheck ()
multiexpr t | t `elem` [Fun,Expr] = return ()
            | otherwise = throwError $ "doesn't have expressions: " ++ show t

ord :: Typ -> QCheck ()
ord t | t `elem` [Int,String,Bool] = return ()
      | otherwise = throwError $ "can't be ordered: " ++ show t

type TypEnv = [(Typ,Typ)]

checkConst :: TypConstraint -> TypEnv -> QCheck ()
checkConst const env = case const of
                         Named a           -> getTyp a >>= named
                         Referencable a    -> getTyp a >>= referencable
                         Typeable a        -> getTyp a >>= typeable
                         MultiLine a       -> getTyp a >>= multiline
                         MultiExpression a -> getTyp a >>= multiexpr
                         Ord a             -> getTyp a >>= ord
    where getTyp a = case lookup a env of
                       Just t  -> return t
                       Nothing -> throwError "constraint error"

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

expect :: Typ -> Typ -> QCheck ()
expect (List a) (List b) = expect a b
expect expected actual 
    | typeVar expected = return ()
    | actual == expected = return ()
    | otherwise = throwError $ "type error: expected: " ++ show expected ++ ", actual: " ++ show actual
