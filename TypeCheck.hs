module TypeCheck where

import Data.Maybe (isJust)
import Control.Monad.Error (throwError,catchError)
import Control.Monad (foldM)
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
check (URef name) e | knownFun name = 
                        do  (f,t) <- getFunType name
                            return $ UFunRef f ::: t
                    | otherwise = 
                        do t <- getVar e name
                           return $ UVarExpr name ::: t
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
  (args', types) <- unzip <$> mapM (getFunType . fname) args
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
  (f', fType) <- getFunType f
  resType <- typeCheck f fType argTypes
  return (f', resType)

typeCheck :: Id -> Typ -> [Typ] -> Either String Typ
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

tooManyParams :: Id -> Int -> Int -> Either String a
tooManyParams f expected actual = throwError $ "too many parameters: " ++ f ++ " (expected " ++ show expected ++ ", actual: " ++ show actual ++ ")" 

tooFewParams :: Id -> Int -> Int -> Either String a
tooFewParams f expected actual = throwError $ "too few parameters: " ++ f ++ " (expected " ++ show expected ++ ", actual: " ++ show actual ++ ")"

type ErrMsg = String

knownFun :: Id -> Bool
knownFun name = isJust . funtype $ name

getFunType :: Id -> Either ErrMsg (UFun, Typ)
getFunType f = maybe (throwError $ "unknown function: " ++ f) return (funtype f)

-- |Associates function name with ast node and function which checks argument types.
funtype :: Id -> Maybe (UFun, Typ)
funtype "functions"   = Just (UFunctions, Mod :->: List Fun)
funtype "name"        = Just (UName, Named A :=>: A :->: String)
funtype "arity"       = Just (UArity, Fun :->: Int)
funtype "loc"         = Just (ULoc, MultiLine A :=>: A :->: List Int)
funtype "null"        = Just (UNull, List A :->: Bool)
funtype "calls"       = Just (UCalls, Fun :->: List Fun)
funtype "path"        = Just (UPath, File :->: FilePath)
funtype "directory"   = Just (UDir, File :->: FilePath)
funtype "filename"    = Just (UFileName, File :->: FilePath)
funtype "file"        = Just (UFile, Mod :->: List File)
funtype "records"     = Just (URecords, File :->: List Record)
funtype "exported"    = Just (UExported, Fun :->: Bool)
funtype "recursivity" = Just (URecursivity, Fun :->: FunRecursivity)
funtype "references"  = Just (UReferences, Referencable A :=>: A :->: List Expr)
funtype "returns"     = Just (UReturns, Fun :->: List Type)
funtype "parameters"  = Just (UParameters, Fun :->: List FunParam)
funtype "type"        = Just (UTypeOf, Typeable A :=>: A :->: Type)
funtype "exprType"    = Just (UExprType, Expr :->: ExprType)
funtype "expressions" = Just (UExpressions, MultiExpression A :=>: A :->: List Expr)
funtype "not"         = Just (UNot, Bool :->: Bool)
funtype "∪"           = Just (UUnion, List A :->: List A :->: List A)
funtype "∈"           = Just (UElem, A :->: List A :->: Bool)
funtype "⊆"           = Just (USubset, List A :->: List A :->: Bool)
funtype "any_in"      = Just (UAnyIn, List A :->: List A :->: Bool)
funtype "origin"      = Just (UOrigin, Expr :->: List Expr)
funtype "reach"       = Just (UReach, Expr :->: List Expr)
funtype "fields"      = Just (UFields, Record :->: List RecordField)
funtype "closureN"    = Just (UClosureN, Int :->: (A :->: List A) :->: A :->: List A)
funtype "lfp"         = Just (ULfp, (A :->: List A) :->: A :->: List A)
funtype "iteration"   = Just (UIteration, Int :->: (A :->: List A) :->: A :->: List A)
funtype "chainN"      = Just (UChainN, Int :->: (A :->: List A) :->: A :->: List (Chain A))
funtype "chainInf"    = Just (UChainInf, (A :->: List A) :->: A :->: List (Chain A)) 
funtype _             = Nothing

relationType :: Binop -> Typ
relationType Regexp = String :->: String :->: Bool
relationType _      = A :->: A :->: Bool

-- |Decides whether the particular type have name function.
named :: Typ -> Either String ()
named t | t `elem` [File,Mod,Fun,Record,RecordField] = return ()
        | otherwise = throwError $ "dont have name: " ++ show t

-- |Decides whether the particular type is referencable.
referencable :: Typ -> Either String ()
referencable t | t `elem` [Fun,Record,RecordField] = return ()
               | otherwise = throwError $ "not referencable: " ++ show t
                             
typeable :: Typ -> Either String ()
typeable t | t `elem` [FunParam,RecordField] = return ()
           | otherwise = throwError $ "not typeable: " ++ show t

multiline :: Typ -> Either String ()
multiline t | t `elem` [File,Mod,Fun] = return ()
            | otherwise = throwError $ "can't count line of codes: " ++ show t

multiexpr :: Typ -> Either String ()
multiexpr t | t `elem` [Fun,Expr] = return ()
            | otherwise = throwError $ "doesn't have expressions: " ++ show t

type TypEnv = [(Typ,Typ)]

checkConst :: TypConstraint -> TypEnv -> Either ErrMsg ()
checkConst const env = case const of
                         Named a           -> getTyp a >>= named
                         Referencable a    -> getTyp a >>= referencable
                         Typeable a        -> getTyp a >>= typeable
                         MultiLine a       -> getTyp a >>= multiline
                         MultiExpression a -> getTyp a >>= multiexpr
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
