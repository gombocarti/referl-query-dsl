{-# LANGUAGE FlexibleContexts #-}

module TypeCheck where

import Data.Maybe (isJust)
import Control.Monad.State
import Control.Monad.Error (throwError,catchError)
import Control.Applicative ((<$>))
import Text.Read (readMaybe)
import Text.Parsec.Pos (SourcePos,newPos)
import Types

getType :: Id -> QCheck Typ
getType v = do 
  env <- get
  case lookup v env of
    Just t  -> return t
    Nothing -> throwError ("not in scope: " ++ v)
               
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

type CallStack = [Id]

type QCheck a =
    StateT Namespace
     (StateT CallStack
       (Either String)) a

runQCheck :: QCheck a -> Namespace -> Either String ((a,Namespace))
runQCheck q ns = evalStateT (runStateT q ns) []
    
type Namespace = [(Id, Typ)]

-- |Type-checks and transforms untyped queries.
check :: UQuery -> QCheck TUQuery
check (UQuery q) = do
  q' ::: t <- check q
  return $ UQuery q' ::: t
check (UBind m (UF x body)) = do
  m' ::: tms <- check m
  expect (Set a) tms `catchError` addExpr m
  let Set tm = tms
  addVar x tm
  body' ::: Set tbody <- check body 
  return $ UBind m' (UF x body') ::: Set tbody
    where a = TypVar "a1"
check (UReturn x) = do
  x' ::: t <- check x
  return (UReturn x' ::: Set t)
check (UTuple xs) = do
  xs' <- mapM check xs
  let (ys,tys) = unzip [(y,ty) | y ::: ty <- xs']
  return $ UTuple ys ::: Tuple tys
check (UWith defs q) = do
  defs' <- mapM check defs
  let ds = [d | d ::: _ <- defs']
  q' ::: tq <- check q
  return $ UWith ds q' ::: tq
check (UFunDef f args body pos) = do
  checkIsDefined f
  fundef ::: ftype <- checkFunDef f args body pos
  addVar f ftype
  return (fundef ::: ftype)
check ref@(URef name) = do
  t <- getType name
  return (ref ::: t)
check (UDataConst cons) =
    case readMaybe cons of 
      Just x -> return $ UExprTypeLit x ::: ExprType
      Nothing -> do
        case readMaybe cons of
          Just x -> return $ UFunRecurLit x ::: FunRecursivity
          Nothing -> throwError $ "unknown literal: " ++ cons
check f@(ULambda x body) = do
  _ ::: t <- checkFunDef "" [x] body (newPos "" 1 1)
  return (f ::: t)
check (UGuard p rest) = do
  _ ::: t <- check p
  expect Bool t
  rest' ::: restT <- check rest
  return (UGuard p rest' ::: restT)
check (UAppExpr f arg) = do
  defining <- getFunDef
  when (recursive f defining) (throwError "recursion is not supported")
  f' ::: fType <- check f
  arg' ::: argt <- check arg
  (appT,_) <- checkApp fType argt
  return $ (UAppExpr f' arg') ::: appT
    where recursive (URef g) defining = g `elem` defining
          recursive _        _        = False
check (UFunComp args) = do
  targs <- mapM check args
  let (args', types) = unzip [(arg, t) | arg ::: t <- targs]
      h:t = reverse types
  compType <- foldM step h t
  return $ UFunComp args' ::: compType
    where
      step :: Typ -> Typ -> QCheck Typ
      step compType atype = fst <$> compose atype compType []
check q@(UNumLit _) = return $ q ::: Int
check q@(UStringLit _) = return $ q ::: String
check q@(UBoolLit _) = return $ q ::: Bool

addExpr :: UQuery -> String -> QCheck ()
addExpr q err = throwError (err ++ "\nin expression " ++ show q)

-- |Checks the argument types.
checkApp :: Typ -> Typ -> QCheck (Typ,TEnv)
checkApp (c :=>: a) argType = do
  res@(a',s) <- checkApp a argType
  checkConst c s
  return res
checkApp fType argType = do
  expect (argType :->: t0) fType
  let (a :->: b) = fType
      s = match a argType
  return (subs s b,s)
    where t0 = TypVar "t0"

match :: Typ -> Typ -> TEnv
match (Set x) (Set y) = match x y
match (Chain x) (Chain y) = match x y
match x       y
    | isTypeVar x = [(x,y)]
    | otherwise   = []

subs :: TEnv -> Typ -> Typ
subs []      y = y
subs [(v,x)] y = subs' y
    where subs' (a :->: b) = subs' a :->: subs' b
          subs' (Set a)    = Set (subs' a)
          subs' (Chain a)  = Chain (subs' a)
          subs' a | a == v = x
                  | otherwise = a

checkIsDefined :: Id -> QCheck ()
checkIsDefined f = do
  namespace <- get
  when (isJust . lookup f $ namespace)
           (throwError ("function already defined: " ++ f))

checkFunDef :: Id -> [Id] -> UQuery -> SourcePos -> QCheck TUQuery
checkFunDef f args body pos = do 
  namespace <- get
  zipWithM_ addArg args [1..]
  setFunDef f
  body' ::: bodyType <- check body `catchError` addLocation
  removeFunDef
  argTypes <- forM args getType
  let ftype = makeFunType argTypes bodyType
  put namespace
  return $ UFunDef f args body' pos ::: ftype
      where 
        addArg arg i     = addVar arg (TypVar ('t':show i))
        addLocation err  = throwError (err ++ "\nin function definition " ++ f ++ "\nin " ++ show pos)

setFunDef :: Id -> QCheck ()
setFunDef f = lift . modify $ (f:)

getFunDef :: QCheck [Id]
getFunDef = lift get

removeFunDef :: QCheck ()
removeFunDef = lift . modify $ tail

makeFunType :: [Typ] -> Typ -> Typ
makeFunType args bodyt = let (cs, ftype) = foldr step ([],bodyt) args
                         in foldr (:=>:) ftype cs
    where step (c :=>: a) (cs, acc) = (c:cs, a :->: acc)
          step arg (cs,acc)         = (cs, arg :->: acc)

type TEnv = [(Typ,Typ)]
{-
-- típuskikövetkeztetés
typeCheck :: UQuery -> Typ -> Typ -> QCheck Typ
typeCheck f t args = fst <$> tcheck t args [] 1
    where
      tcheck :: Typ -> [Typ] -> TEnv-> Int -> QCheck (Typ,TEnv)
      tcheck a@(_ :->: _) [] env _ind =
          return (subs a env,env)
      tcheck a@(_ :->: _) ((constr :=>: c) : xs) env ind = do
        res@(_,env') <- tcheck a (c : xs) env ind
        checkConst constr env'
        return res
      tcheck (a :->: b) (x:xs) env ind = do
        env' <- unify a x env ind
        tcheck b xs env' (ind + 1)
      tcheck (constr :=>: a) args env ind = do
        res@(a',env') <- tcheck a args env ind
        let v = getTypeVar constr
        if isJust . lookup v $ env'
        then do
          checkConst constr env'
          return res
        else return (constr :=>: a',env')
      tcheck _ (_:_) _env _ind = 
          tooManyParams f argsCount (length args)
      tcheck (Set a) [] env ind = do
        (aType, _) <- tcheck a [] env ind
        return (Set aType, env)
      tcheck (Chain a) [] env ind = do
        (aType, _) <- tcheck a [] env ind
        return (Chain aType, env)      
      tcheck a [] env _ind
          | isTypeVar a = case lookup a env of
                          Just b -> return (b,env)
                          Nothing -> return (a,env)
          | otherwise = return (a,env)

      unify :: Typ -> Typ -> TEnv -> Int -> QCheck TEnv
      unify t1@(a :->: b) t2@(c :->: d) env ind =
          catchError
          (do
            env' <- unify a c env ind
            unify b d env' ind)
          (\_ -> throwError $ errorMsg t1 t2 ind)
      unify (Set a) (Set b) env ind = unify a b env ind
      unify a b@(TypVar _) env ind 
          | isTypeVar a = do let tb = case lookup a env of
                                      Just ta  -> ta
                                      Nothing  -> argType t ind
                           setType b tb
                           return $ (a,b):env
          | otherwise = do setType b a
                           return ((b,a):env)
      unify a b env ind
          | isTypeVar a  = case lookup a env of 
                           Just at -> unify at b env ind
                           Nothing -> do
                             setType a b
                             return $ (a,b):env
          | a == b     = return env
          | otherwise  = throwError $ errorMsg a b ind

      errorMsg e a ind = "expected: " ++ show e ++ " actual: " ++ show a ++ "\nat the " ++ show ind ++ ". argument of " ++ f -- todo namely, show arg

      argsCount = fArgs t 0

      fArgs :: Typ -> Int -> Int
      fArgs (_ :=>: b) n = fArgs b n
      fArgs (_ :->: b) n = fArgs b (n + 1)
      fArgs _          n = n

      subs :: Typ -> TEnv -> Typ
      subs (a :->: b) env = subs a env :->: subs b env
      subs (Chain a) env = Chain (subs a env)
      subs (Set a) env = Set (subs a env)
      subs a env | isTypeVar a = case lookup a env of
                                 Just t -> t
                                 Nothing -> a
                 | otherwise = a
-}
getTypeVar (Named a) = a
getTypeVar (Referencable a) = a
getTypeVar (Typeable a) = a
getTypeVar (MultiLine a) = a
getTypeVar (MultiExpression a) = a
getTypeVar (MultiSet a) = a
getTypeVar (Ord a) = a

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

type ErrMsg = String

-- |Associates function name with type.
funtypes :: [(Id, Typ)]
funtypes = relType ++
    [ ("modules", Set Mod)
    , ("files", Set File)
    , ("atFile", File)
    , ("atModule", Mod)
    , ("atFunction", Fun)
    , ("atExpr", Expr)
    , ("atField", RecordField)
    , ("functions", Mod :->: Set Fun)
    , ("name", Named a :=>: a :->: String)
    , ("arity", Fun :->: Int)
    , ("loc", MultiLine a :=>: a :->: Set Int)
    , ("null", Set a :->: Bool)
    , ("calls", Fun :->: Set Fun)
    , ("callsP", (Fun :->: Bool) :->: Fun :->: Set Fun)
    , ("path",  File :->: FilePath)
    , ("directory", File :->: FilePath)
    , ("filename", File :->: FilePath)
    , ("file",  Mod :->: Set File)
    , ("module", File :->: Set Mod)
    , ("defmodule", Fun :->: Set Mod)
    , ("records",  File :->: Set Record)
    , ("exported", Fun :->: Bool)
    , ("recursivity",  Fun :->: FunRecursivity)
    , ("references", Referencable a :=>: a :->: Set Expr)
    , ("returns", Fun :->: Set Type)
    , ("parameters",Fun :->: Set FunParam)
    , ("type",  Typeable a :=>: a :->: Type)
    , ("exprType", Expr :->: ExprType)
    , ("exprValue", Expr :->: String)
    , ("expressions", MultiExpression a :=>: a :->: Set Expr)
    , ("subexpressions", Expr :->: Set Expr)
    , ("index", Expr :->: Int)
    , ("not", Bool :->: Bool)
    , ("~=", String :->: String :->: Bool)
    , ("||", Bool :->: Bool :->: Bool)
    , ("∪", Set a :->: Set a :->: Set a)
    , ("∈", a :->: Set a :->: Bool)
    , ("⊆", Set a :->: Set a :->: Bool)
    , ("any_in", Set a :->: Set a :->: Bool)
    , ("origin", Expr :->: Set Expr)
    , ("reach", Expr :->: Set Expr)
    , ("fields", Record :->: Set RecordField)
    , ("record", RecordField :->: Record)
    , ("closureN", Int :->: (a :->: Set a) :->: a :->: Set a)
    , ("lfp", (a :->: Set a) :->: a :->: Set a)
    , ("iteration", Int :->: (a :->: Set a) :->: a :->: Set a)
    , ("chainN", Int :->: (a :->: Set a) :->: a :->: Set (Chain a))
    , ("chainInf", (a :->: Set a) :->: a :->: Set (Chain a))
    , ("max", Ord a :=>: Set a :->: Set a)
    , ("min", Ord a :=>: Set a :->: Set a)
    , ("average", Set Int :->: Set Int)
    , ("count", Chain a :->: Int)
    , ("distinct", MultiSet a :=>: a :->: a)
    , ("groupBy", (a :->: b) :->: Set a :->: Grouped a b)
--    , ("const", a :->: b :->: a)
    ]
    where a       = TypVar "a"
          b       = TypVar "b"
          rels    = ["==","/=","<","<=",">",">="]
          relType = [(rel, a :->: a :->: Bool) | rel <- rels]

multiset :: Typ -> QCheck ()
multiset (Set _) = return ()
multiset (Chain _) = return ()
multiset t = throwError $ "is not multiset: " ++ show t

-- |Decides whether the particular type have name function.
named :: Typ -> QCheck ()
named (TypVar _)             = return ()
named t | t `elem` namedInst = return ()
        | otherwise          = throwError $ "doesn't have name: " ++ show t
    where
      namedInst = [File,Mod,Fun,Record,RecordField]
                      
-- |Decides whether the particular type is referencable.
referencable :: Typ -> QCheck ()
referencable (TypVar _)    = return ()
referencable t | inst      = return ()
               | otherwise = throwError $ "not referencable: " ++ show t
    where
      inst          = t `elem` referAbleInst
      referAbleInst = [Fun,Record,RecordField]
                          
typeable :: Typ -> QCheck ()
typeable (TypVar _)    = return ()
typeable t | inst      = return ()
           | otherwise = throwError $ "not typeable: " ++ show t
    where
      inst         = t `elem` typeAbleInst
      typeAbleInst = [FunParam,RecordField]

multiline :: Typ -> QCheck ()
multiline (TypVar _) = return ()
multiline t | t `elem` [File,Mod,Fun] = return ()
            | otherwise = throwError $ "can't count line of codes: " ++ show t

multiexpr :: Typ -> QCheck ()
multiexpr (TypVar _) = return ()
multiexpr t | t `elem` [Fun,Expr] = return ()
            | otherwise = throwError $ "doesn't have expressions: " ++ show t

ord :: Typ -> QCheck ()
ord (TypVar _) = return ()
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
                         MultiSet a        -> getTyp a >>= multiset
                         Ord a             -> getTyp a >>= ord
    where getTyp a = case lookup a env of
                       Just t  -> return t
                       Nothing -> throwError ("constraint error: " ++ show a)

compose :: Typ -> Typ -> TEnv ->  QCheck (Typ,TEnv)
compose (const :=>: f) g@(a :->: b) env = do
  (t, env') <- compose f g env
  checkConst const env'
  return (t, env')
compose (c :->: d) (a :->: b) env 
    | isTypeVar c = do expect (Set c) b
                       let (Set t) = b
                       return $ (a :->: d, (c,t):env)
                             
    | otherwise = do  expect (Set c) b
                      return $ (a :->: d, env)
              
resultType :: Typ -> Typ
resultType (_ :=>: b) = resultType b
resultType (_ :->: b) = resultType b
resultType b = b

{-
argType :: Typ -> Typ
argType (c :=>: b) = c :=>: argType b
argType (a :->: b) | funType b = a :->: argType b
                   | otherwise = a
-}
isFunType :: Typ -> Bool
isFunType (_ :=>: b) = isFunType b
isFunType (_ :->: _) = True
isFunType _          = False

isTypeVar :: Typ -> Bool
isTypeVar (TypVar _) = True
isTypeVar _          = False

expect :: Typ -> Typ -> QCheck ()
expect (Set a) (Set b) = expect a b
expect (a :->: b) (c :->: d) = do {expect a c; expect b d}
expect expected actual 
    | isTypeVar expected = return ()
    | isTypeVar actual   = return ()
    | actual == expected = return ()
    | otherwise = throwError $ "type error: expected: " ++ show expected ++ ", actual: " ++ show actual
