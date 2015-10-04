module SqRefact
    (eval
    ,runEval
    ,showValue'
    ,initErl
    ,Database
    ,Arg
    ,curried)
where

import Prelude hiding (seq,mod)
import Types (Id,Query(..),LF(..),ExprType,FunctionType)
import Foreign.Erlang
import Data.List (union,nub,groupBy,intercalate,partition,intersect)
import Text.Regex.Posix ((=~))
import Control.Monad.Reader
import Control.Monad.State
import Data.Functor ((<$>))
import Data.Function (on)
import System.FilePath (takeFileName,takeDirectory)
import Data.String.Utils (strip)
import Sq (subset,Chain(..))

import Data.Char (toUpper,toLower)

import Pipes
import qualified Pipes.Prelude as Pr

data Value
    = File { getFile :: ErlType }
    | Mod ErlType
    | Fun ErlType
    | Expr ErlType
    | Type ErlType
    | TypeExpr ErlType
    | FunParam ErlType
    | Rec ErlType
    | RecField ErlType
    | ExprType ExprType
    | FunRecursivity FunctionType
    | String String
    | Int Int
    | Bool Bool
    | Unit
    | Path FilePath
    | Chain (Chain Value)
    | Seq [Value]
    | SeqP (Producer Value (StateT EvalState IO) ())
    | Grouped [(Value,Value)]
    | Tuple [Query] [Value]
    | FunDef [Id] [(Id,Value)] Query
--    | Lambda Id Query
    | Curried Id [Value]
--      deriving (Show,Eq)

instance Eq Value where
    (Mod a) == (Mod b) = a == b
    (Int a) == (Int b) = a == b

instance Show Value where
    show (Mod m) = "Mod " ++ show m
    show (Fun f) = "Fun " ++ show f

curried :: Id -> Value
curried f = Curried f []

instance Erlang Value where
    toErlang = valueToErlang
    fromErlang = erlangToValue

valueToErlang :: Value -> ErlType
valueToErlang (File f)     = f
valueToErlang (Mod m)      = m
valueToErlang (Fun f)      = f
valueToErlang (Expr e)     = e
valueToErlang (Rec r)      = r
valueToErlang (Seq xs)     = toErlang xs
valueToErlang (Grouped xs) = toErlang xs
valueToErlang (ExprType t) = ErlAtom . lowercase . show $ t
    where lowercase (c:s) = toLower c : s
          lowercase ""    = error "lowercase: empty string"
valueToErlang (RecField f) = f
--valueToErlang v            = error ("valueToErlang: " ++ show v)

erlangToValue :: ErlType -> Value
erlangToValue = undefined

instance Ord Value where
    (Int a)     <= (Int b)     = a  <= b
    (String s1) <= (String s2) = s1 <= s2
    (Bool a)    <= (Bool b)    = a  <= b

lib_file        :: String
lib_module      :: String
lib_function    :: String            
lib_record      :: String
lib_recordfield :: String
lib_spec        :: String
lib_type        :: String
lib_typeExp     :: String
lib_clause      :: String
lib_form        :: String
lib_expr        :: String
lib_dynfun      :: String
lib_args        :: String
lib_haskell     :: String
lib_sq          :: String
dataflow        :: String
query_lib       :: String
metrics         :: String
syntax          :: String
               
lib_file        = "reflib_file"
lib_module      = "reflib_module"
lib_function    = "reflib_function"
lib_record      = "reflib_record"
lib_recordfield = "reflib_record_field"
lib_spec        = "reflib_spec"
lib_type        = "reflib_type"
lib_typeExp     = "reflib_typexp"
lib_clause      = "reflib_clause"
lib_form        = "reflib_form"
lib_expr        = "reflib_expression"
lib_dynfun      = "reflib_dynfun"
lib_args        = "reflib_args"
lib_haskell     = "reflib_haskell"
lib_sq          = "refusr_sq_lib"
dataflow        = "refanal_dataflow"
query_lib       = "reflib_query"
metrics         = "refusr_metrics"
syntax          = "refcore_syntax"

filepath     :: ErlFunction -> GraphPath
modpath      :: ErlFunction -> GraphPath
funpath      :: ErlFunction -> GraphPath
clausepath   :: ErlFunction -> GraphPath
formpath     :: ErlFunction -> GraphPath
recpath      :: ErlFunction -> GraphPath
recfieldpath :: ErlFunction -> GraphPath
specpath     :: ErlFunction -> GraphPath
dynfunpath   :: ErlFunction -> GraphPath
                  
filepath     = GPath lib_file
modpath      = GPath lib_module
funpath      = GPath lib_function
clausepath   = GPath lib_clause
formpath     = GPath lib_form
recpath      = GPath lib_record
recfieldpath = GPath lib_recordfield
specpath     = GPath lib_spec
dynfunpath   = GPath lib_dynfun

type FilePosition = Int

type Arg = (FilePath,FilePosition)

getArg' :: Eval (Maybe Arg)
getArg' = gets args
    
getArg :: Eval ErlType
getArg = do
  arg <- getArg'
  case arg of 
    Just (file,pos) -> return $ refactArg file pos
    Nothing         -> throwError "no file and position are given"
  where
    refactArg file pos = ErlList
         [ erlPair (ErlAtom "ask_missing") (ErlAtom "false")
         , erlPair (ErlAtom "file") file
         , erlPair (ErlAtom "position") pos
         ]
    
    erlPair :: (Erlang a, Erlang b) => a -> b -> ErlType
    erlPair a b = ErlTuple [toErlang a,toErlang b]

type ErlModule = String
type ErlFunction = String

data GraphPath = GPath ErlModule ErlFunction
               | GSeq [GraphPath]
               | All [GraphPath]
                 deriving Show

newtype Database = Database 
    { call :: ErlModule -> ErlFunction -> [ErlType] -> IO ErlType }
              
initErl :: String -> IO Database
initErl name = do 
  self <- createSelf name
  mbox <- createMBox self
  return $ Database { call = rpcCall mbox referl }
      where referl :: Node
--            referl = Long "refactorerl" "192.168.0.17"
            referl = Short "refactorerl"


type Env = [(Id, Value)]

data EvalState = EvalState
    { env  :: Env
    , db   :: Database
    , args :: Maybe Arg
    }

type Eval a = StateT EvalState IO a

runEval :: Eval a ->  Database -> Maybe Arg -> Env -> IO a
runEval q db arg env = evalStateT q initState
    where initState = EvalState env db arg

putEnv :: Env -> Eval ()
putEnv newEnv = do
  s <- get
  put (s { env = newEnv })
                      
eval :: Query -> Eval Value
eval (Query q) = eval q
eval (BindE m (Lambda x body)) = do
  Seq as <- eval m
  env <- getEnv
  (return . SeqP) (for (each as) (f env))
      where 
        f env = (\a -> do
                   lift (putEnv ((x,a):env))
                   xs <- lift (eval body)
                   case xs of
                     Seq ys -> each ys
                     SeqP ys -> ys
                )
eval (ReturnE x) = do
  y <- eval x
  seq [y]
eval (GuardE p) = do
  Bool res <- eval p
  if res
  then seq [Unit]
  else seq []
{-eval (UGroupBy f q) = do
  Seq xs <- eval q
  ys <- forM xs (evalApp' f)
  let zs = zip ys xs
  return $ Grouped (group zs)
    where 
      group xs = map merge (groupBy criteria xs)
      criteria = (==) `on` fst
      merge xs = (fst . head $ xs, Seq (map snd xs))
-}
--eval (UVarExpr v) env = readVar v env
{-
eval (AppE "closureN" [n,fs,x]) = do
  Int n' <- eval n
  x' <- eval x
  Seq <$> closureNM n' f x'
    where f      = makeFun fs
eval (AppE "lfp" [fs,x]) = do
  x' <- eval x
  Seq <$> lfpM f x'
    where f = makeFun fs
eval (AppE "iteration" [n,fs,x]) = do
  Int n' <- eval n
  x' <- eval x
  Seq <$> iterationM n' f x'
    where f      = makeFun fs
eval (AppE "chainInf" [fs,x]) = do 
  x' <- eval x
  Seq <$> chainInfM f x'
    where f = makeFun fs
eval (AppE "chainN" [n,fs,x]) = do
  Int n' <- eval n
  x' <- eval x
  Seq <$> chainNM n' f x'
    where f = makeFun fs
-}
eval (RefE "modules") = queryDb Mod (modpath "all")
eval (RefE "files") = queryDb File (filepath "all")
eval (RefE "atFunction") = do
  arg <- getArg
  f <- callDb' lib_args "function" [arg] (throwError "atFunction: no function at given position")
  return . Fun $ f
eval (RefE "atFile") = do
  arg <- getArg
  f <- callDb' lib_args "file" [arg] (throwError "atFile: no file at given position")
  return . File $ f
eval (RefE "atModule") = do
  arg <- getArg
  m <- callDb' lib_args "module" [arg] (throwError "atModule: no module at given position")
  return . Mod $ m
eval (RefE "atExpr") = do
  arg <- getArg
  e <- callDb' lib_args "expression" [arg] (throwError "atExpression: no expression at given position")
  return . Expr $ e
eval (RefE "atField") = do
  arg <- getArg
  f <- callDb' lib_args "record_field" [arg] (throwError "atField: no record field at given position")
  return . RecField $ f
eval (TupleE components) = do
  xs <- mapM eval components
  return $ Tuple components xs
eval (RefE name) = readVar name
--eval (ULambda x body) = return (Lambda x body)
eval (WithE defs q) = addFuns >> eval q
    where
      addFuns = forM defs addFun
      addFun (FunDefE f args body _) = modifyEnv ((f,FunDef args [] body):)
eval (AppE (AppE (AppE (RefE "iteration") n) fs) x) = do
  Int n' <- eval n
  x' <- eval x
  Seq <$> iterationM n' f x'
    where f      = makeFun fs
eval (AppE (RefE f) arg) = do
  arg' <- eval arg
  fun <- readVar f
  evalApp fun arg'
eval (AppE f arg) = do
    f' <- eval f
    arg' <- eval arg
    evalApp f' arg'
eval (StringLitE s) = string s
eval (NumLitE i) = int i
eval (BoolLitE b) = bool b
eval (ExprTypeLitE t) = return $ ExprType t
eval (FunRecurLitE fr) = return $ FunRecursivity fr
eval x = error (show x)

-- noob
erlError :: ErlType -> Bool
erlError (ErlTuple (ErlAtom x:_)) = x `elem` ["badrpc","reflib_error"]
erlError _ = False

getDb :: Eval Database
getDb = gets db
             
callDb :: ErlModule -> ErlFunction -> [ErlType] -> Eval ErlType
callDb mod fun args = do
  db <- getDb
  liftIO $ call db mod fun args

callDb' :: ErlModule -> ErlFunction -> [ErlType] -> Eval ErlType -> Eval ErlType
callDb' mod fun args x = do
  db <- getDb
  y <- liftIO $ call db mod fun args
  if erlError y
  then x
  else return y

queryDb :: (ErlType -> Value) -> GraphPath -> Eval Value
queryDb f p = do
  p' <- evalPath p
  x <- callDb query_lib "exec" [p']
  wrap f x

queryDb1 :: (ErlType -> Value) -> GraphPath -> ErlType -> Eval Value
queryDb1 f p x = do 
  p' <- evalPath p
  y <- callDb query_lib "exec" [x,p']
  wrap f y

queryDb1' :: (ErlType -> Value) -> ErlModule-> ErlFunction -> ErlType -> Eval Value
queryDb1' f mod fun x = do
  y <- callDb mod fun [x]
  wrap f y

propertyDb :: Erlang a => (a -> Value) -> ErlModule -> ErlFunction -> ErlType -> Eval Value
propertyDb f mod fun arg = do 
  x <- callDb mod fun [arg]
  return . f . fromErlang $ x

evalPath :: GraphPath -> Eval ErlType
evalPath (GPath mod fun) = callDb mod fun []
evalPath (GSeq xs) = do
  ps <- mapM evalPath xs
  callDb query_lib "seq" [ErlList ps]
evalPath (All xs) = do
  ps <- mapM evalPath xs
  callDb query_lib "all" [ErlList ps]

wrap :: (ErlType -> Value) -> ErlType -> Eval Value
wrap _f ErlNull     = return . Seq $ []
wrap f (ErlList xs) = return . Seq . map f $ xs
wrap f x            = return . f $ x

evalRel :: Value -> Id -> Value -> Bool
evalRel a "==" b = a == b
evalRel a "/=" b = a /= b
evalRel a ">"  b = a >  b
evalRel a ">=" b = a >= b
evalRel a "<"  b = a <  b
evalRel a "<=" b = a <= b
evalRel a "=~" b = s =~ regex
    where String s     = a
          String regex = b
evalRel _ s    _ = error ("evalRel: unknown relation: " ++ s)
                   
makeFun :: Query -> Value -> Eval [Value]
makeFun (RefE f) v = do Seq xs <- evalApp' f v
                        return xs
makeFun app@(AppE _ _) v = do f <- eval app
                              Seq xs <- evalApp f v
                              return xs
makeFun (FunCompE args) v = do Seq xs <- foldM step (Seq [v]) (reverse args)
                               return xs
    where 
      step (Seq xs) (RefE f) = concatValueM $ mapM (evalApp' f) xs
      step (Seq xs) app@(AppE _ _) = do
        f <- eval app
        xs' <- mapM (evalApp f) xs
        return $ concatValue xs'
      step (Seq _) _ = error "step: not function argument"
      step _       _ = error "step: not sequence argument"
makeFun _ _ = error "makeFun: not fuction argument"

lfpM :: (Value -> Eval [Value]) -> Value -> Eval [Value]
lfpM f x = loop [] [x]
    where 
      loop :: [Value] -> [Value] -> Eval [Value]
      loop old curr = do
        let all = union old curr
        new <- mapM f curr
        let new' = concat new
        if new' `subset` all
        then return all
        else loop all new'            

closureNM :: Int -> (Value -> Eval [Value]) -> Value -> Eval [Value]
closureNM n f x = loop n [x]
    where 
      loop 0 xs = return xs
      loop m xs = do
        xs' <- concat <$> mapM f xs
        loop (m - 1) (union xs xs')  -- foldM

iterationM :: Int -> (Value -> Eval [Value]) -> Value -> Eval [Value]
iterationM n f x = loop n [x]
    where loop 0 xs = return xs
          loop m xs = do
            xs' <- concat <$> mapM f xs
            loop (m - 1) xs'

chainNM :: Int -> (Value -> Eval [Value]) -> Value -> Eval [Value]
chainNM n f x = do chains <- loop n [] [Incomplete [x]]
                   return $ map Chain chains
    where loop 0 finished unfinished = return (unfinished ++ finished)
          loop _ finished []         = return finished
          loop m finished unfinished = do
            new <- concat <$> mapM (cont f) unfinished
            let (unfinished',finished') = split new
            loop (m - 1) (finished' ++ finished) unfinished'
            

chainInfM :: (Value -> Eval [Value]) -> Value -> Eval [Value]
chainInfM f x = do finished <- loop [] [Incomplete [x]]
                   return $ map Chain finished
    where loop finished []         = return finished
          loop finished unfinished = do
            new <- concat <$> mapM (cont f) unfinished
            let (unfinished', finished') = split new
            loop (finished' ++ finished) unfinished'

cont :: Eq a => (a -> Eval [a]) -> Chain a -> Eval [Chain a]
cont f (Incomplete chain@(z:_)) = do
  xs <- f z
  case xs of
    [] -> return [Complete chain]
    ys -> return [classify y chain | y <- ys]
  where 
    classify :: Eq a => a -> [a] -> Chain a
    classify y chain | y `elem` chain = Recursive chain
                     | otherwise      = Incomplete (y:chain)
cont _ chain = return [chain]

split :: [Chain a] -> ([Chain a],[Chain a])
split chains = partition isInComplete chains 

isInComplete :: Chain a -> Bool
isInComplete (Incomplete _) = True
isInComplete _              = False

concatValue :: [Value] -> Value
concatValue vals = Seq $ foldr step [] vals
    where step (Seq xs) acc = xs ++ acc

concatValueM :: Eval [Value] -> Eval Value
concatValueM m = do
  xs <- m
  return $ concatValue xs

maybeReadVar :: Id  -> Eval (Maybe Value)
maybeReadVar v = do 
  env <- getEnv
  return $ lookup v env

readVar :: Id -> Eval Value
readVar v = do
  env <- getEnv
  case lookup v env of
    Just val -> return val
    Nothing  -> throwError ("undefined variable: " ++ v)

throwError = error
                
section :: Id -> Value
section f = Curried f []

evalApp' :: Id -> Value -> Eval Value
evalApp' f arg = evalApp (section f) arg

evalApp :: Value -> Value -> Eval Value
evalApp (Curried f [p1]) p2 | isRelation = bool (evalRel p1 f p2)
                              where isRelation = f `elem` ["==","<",">","/="]
evalApp (Curried "name" []) arg = 
    case arg of
      Fun f      -> propertyDb String lib_function "name" f
      Mod m      -> propertyDb String lib_module "name" m
      File _     -> evalApp' "filename" arg
      Rec r      -> propertyDb String lib_record "name" r
      TypeExpr t -> propertyDb String lib_typeExp "name" t
      Type t     -> propertyDb String lib_type "name" t
-- TODO mit ad vissza a returntypes refactorerlben? (type vagy typeexpr?)
evalApp (Curried "includes" []) (File f) =
    queryDb1 File (filepath "includes") f
evalApp (Curried "arity" []) (Fun f) = propertyDb Int lib_function "arity" f
evalApp (Curried "loc" []) arg   = propertyDb Int metrics "metric" args
    where 
      args = ErlTuple [ErlAtom "line_of_code",ErlAtom typeTag,x]
      (typeTag, x) = case arg of
                       Fun f  -> ("function",f)
                       File f -> ("file",f)                      
evalApp (Curried "not" []) (Bool p) = bool . not $ p
evalApp (Curried "||" [Bool x]) (Bool y) = bool (x || y) 
evalApp (Curried "null" []) (SeqP xs) = Bool <$> Pr.null xs
evalApp (Curried "null" []) (Seq xs) = bool . null $ xs
evalApp (Curried "==" [x]) y = bool (x == y)
evalApp (Curried "/=" [x]) y = bool (x /= y)
evalApp (Curried ">" [x]) y = bool (x > y)
evalApp (Curried ">=" [x]) y = bool (x >= y)
evalApp (Curried "<" [x]) y = bool (x < y)
evalApp (Curried "<=" [x]) y = bool (x <= y)
evalApp (Curried "∈" [a]) (Seq bs) = bool $ a `elem` bs
evalApp (Curried "⊂" [Seq as]) (Seq bs) = bool $ as `all_in` bs
evalApp (Curried "any_in" [Seq as]) (Seq bs) = bool $ as `any_in` bs
evalApp (Curried "∪" [Seq as]) (Seq bs) = seq $ as `union` bs
evalApp (Curried "calls" []) arg = evalApp (Curried "callsP" [p]) arg
    where p = (Curried "const" [Bool True])
evalApp (Curried "callsP" [p]) (Fun f) = do 
  Seq funs <- queryDb1 Fun (funpath "funcalls") f
  funs' <- filterM pred funs
  seq funs'
    where 
      pred :: Value -> Eval Bool
      pred fun = do Bool b <- evalApp p fun
                    return b
evalApp (Curried "functions" []) (Mod m) = queryDb1 Fun (modpath "locals") m
evalApp (Curried "records" []) (File f) = queryDb1 Rec (filepath "records") f
evalApp (Curried "exported" []) (Fun f) = propertyDb Bool lib_function "is_exported" f
evalApp (Curried "file" []) (Mod m) = queryDb1 File (modpath "file") m
evalApp (Curried "defmodule" []) (Fun f) = do
  Seq [m] <- queryDb1 Mod (funpath "module") f
  return m
evalApp (Curried "module" []) (File f) = queryDb1 Mod (filepath "module") f
evalApp (Curried "path" []) (File f) = propertyDb String lib_file "path" f
evalApp (Curried "dir" []) (f) = do 
  String path <- evalApp' "path" f
  string . takeDirectory $ path
evalApp (Curried "filename" []) f = do
  String path <- evalApp' "path" f
  string . takeFileName $ path
{-
evalApp UTypeOf [arg) = 
    case arg of 
      FunParam p -> callDb' 
      RecField f -> queryDb1 f URecFieldTypeOf Type
-}
-- evalApp UExprType [Expr e) = queryDb1 e UExprType ExprType

-- evalApp URecursivity [Fun f) = wrap . Sq.frecursive $ f

evalApp (Curried "returns" []) (Fun f) = queryDb1 Type path f
    where path = GSeq [ funpath "spec"
                      , specpath "returntypes"
                      ] -- todo: typexp -> namedtype konverzio
evalApp (Curried "origin" []) (Expr expr) = do
  es <- callDb dataflow "reach" args
  wrap Expr es
    where args = [ErlList [expr], ErlList [ErlAtom "back"]]
evalApp (Curried "fields" []) (Rec r) = queryDb1 RecField (recpath "fields") r
evalApp (Curried "record" []) (RecField f) = do
  Seq [record] <- queryDb1 Rec (recfieldpath "recorddef") f
  return record
evalApp (Curried "references" []) (Fun f) =
    queryDb1' Expr lib_haskell "function_references" f
evalApp (Curried "references" []) (Rec f) = 
    queryDb1 Expr (recpath "references") f
evalApp (Curried "references" []) (RecField f) =
    queryDb1 Expr (recfieldpath "references") f
evalApp (Curried "expressions" []) (Fun f) = queryDb1 Expr path f
    where 
      path = GSeq [ funpath "definition"
                  , formpath "clauses"
                  , clausepath "exprs"
                  ]
evalApp (Curried "subexpressions" []) (Expr e) = 
    queryDb1' Expr lib_haskell "subexpressions" e
evalApp (Curried "exprType" []) (Expr e) =
    queryDb1' (ExprType . read . capitalize . fromErlang) lib_expr "type" e
    where capitalize (c:s) = toUpper c : s
          capitalize ""    = error "capitalize: empty string"
evalApp (Curried "exprValue" []) (Expr e) =
    queryDb1' (String . fromErlang) lib_haskell "expr_value" e
evalApp (Curried "exprParams" []) (Expr e) =
    queryDb1' Expr lib_sq "expr_param" e
evalApp (Curried "index" []) (Expr e) =
    queryDb1' (Int . fromErlang) lib_sq "expr_index" e
evalApp (Curried "max" []) (Seq xs) = seq [maximum xs]
evalApp (Curried "min" []) (Seq xs) = seq [minimum xs]
evalApp (Curried "average" []) (Seq xs) = seq . map Int . average $ ns
    where ns = [n | Int n <- xs]

evalApp (Curried "length" []) (Chain c) = int . length . getChain $ c
evalApp (Curried "distinct" []) (Chain c) = chain $ fChain nub c
evalApp (Curried "distinct" []) (Seq xs) = seq $ nub xs
evalApp (Curried "const" [a]) (_) = return a
evalApp (Curried "groupBy" [f]) (Seq xs) = do
  ys <- forM xs (evalApp f)
  let zs = zip ys xs
  return $ Grouped (group zs)
    where 
      group ys = map merge (groupBy criteria ys)
      criteria = (==) `on` fst
      merge ys = (fst . head $ ys, Seq (map snd ys))
evalApp (Curried f args) arg = return $ Curried f (args ++ [arg])
evalApp (FunDef argNames ps body) param =
    let (defined:argNames') = argNames
        defined' = (defined,param):ps
    in
      case argNames' of
        [] -> do
          env <- getEnv
          modifyEnv (defined' ++)
          result <- eval body
          putEnv env
          return result
        _  -> return (FunDef argNames' defined' body)
evalApp _ _ = error "evalApp: not function"

getEnv :: Eval Env
getEnv = gets env

modifyEnv :: (Env -> Env) -> Eval ()
modifyEnv f = do
  env <- getEnv
  putEnv . f $ env

string :: String -> Eval Value
string = return . String

int :: Int -> Eval Value
int = return . Int

bool :: Bool -> Eval Value
bool = return . Bool

chain :: Sq.Chain Value -> Eval Value
chain = return . Chain

seq :: [Value] -> Eval Value
seq = return . Seq

fChain :: ([a] -> [b]) -> Sq.Chain a -> Sq.Chain b
fChain f (Sq.Incomplete xs) = Sq.Incomplete $ f xs
fChain f (Sq.Complete xs)   = Sq.Complete   $ f xs
fChain f (Sq.Recursive xs)  = Sq.Recursive  $ f xs

getChain :: Sq.Chain a -> [a]
getChain (Sq.Incomplete xs) = xs
getChain (Sq.Complete xs)   = xs
getChain (Sq.Recursive xs)  = xs

flatten :: Value -> [Value] -> [Value]
flatten (Seq xs@(Seq _:_)) ys = foldr flatten ys xs
flatten (Seq xs)           ys = xs ++ ys
flatten x                  ys = x : ys

showValue' :: Value -> Eval String
showValue' (SeqP xs) = Pr.toListM xs >>= showValue' . Seq
showValue' (Seq []) = return ""
showValue' (Seq xs@(Tuple _ _:_)) = showTuples xs
showValue' xs@(Seq _) = fromErlang <$> callDb lib_haskell "name" [xs']
    where xs' = toErlang xs
showValue' g@(Grouped _) = do
  xs <- fromErlang <$> callDb lib_haskell "name" [g']
  return . format $ xs
    where g'  = toErlang g

          format :: [(String,[String])] -> String
          format = foldr (\group acc -> formatGroup group ++ "\n" ++ acc) ""

          formatGroup :: (String,[String]) -> String
          formatGroup (grouping,names) = grouping ++ "\n" ++ unlines ["   " ++ name | name <- names]
showValue' (Bool b) = return . show $ b
showValue' (Int n)  = return . show $ n
showValue' (String s) = return s
showValue' (ExprType et) = return . show $ et
showValue' x        = showValue' (Seq [x])
          

showValue :: Value -> Eval String
showValue f@(File _)   = do
  String name <- evalApp' "filename" f
  return name  
showValue m@(Mod _)    = do 
  String s <- evalApp' "name" m
  return s
{-         
showValue f@(Fun _)    = do 
  String name <- evalApp' "name" f
  Int arity <- evalApp' "arity" f
  defmod <- evalApp' "defmodule" f
  prefix <- case defmod of
              Seq []    -> return ""
              Seq [mod] -> do
                           String modname <- evalApp' "name" mod
                           return $ modname ++":"
  return $ prefix ++ name ++ "/" ++ show arity
-}
showValue (Fun f)    = do
  ErlString name <- callDb lib_haskell "name" [ErlList [f],tag]
  return name
  where
    tag = ErlAtom "function"
showValue r@(Rec _)    = do
  String s <- evalApp' "name" r
  return s
showValue t@(Type _)   = do
  String s <- evalApp' "name" t
  return s
showValue (Expr e)     = do
  s <- callDb syntax "flat_text2" [e]
  return . strip . fromErlang $ s
showValue (Int n)      = return . show $ n
showValue (String s)   = return s
showValue (Bool b)     = return . show $ b
showValue (Seq xs@((Tuple _ _):_)) = showTuples xs  
showValue (Seq xs)     = unlines <$> mapM showValue xs
showValue (Grouped xs) = unlines <$> mapM showGroup xs
    where showGroup (x,ys) = do
            sx <- showValue x
            sys <- showValue ys
            return $ sx ++ unlines ["  " ++ sy | sy <- words sys]
showValue (Chain (Incomplete chain)) = do
  s <- mapM showValue (reverse chain)
  return $ unwords s ++ " ..."
showValue (Chain (Complete chain)) = do
  s <- mapM showValue (reverse chain)
  return $ unwords s
showValue (Chain (Recursive chain)) = do
  s <- mapM showValue (reverse chain)
  return $ unwords s ++ " *"

showTuples :: [Value] -> Eval String
showTuples ts@((Tuple components _):_) = do
  let header = map show components
  lines <- mapM showLine ts
  let defaultWidth = map ((+2) . length) header
      colWidths = colWidth lines defaultWidth
      ids'   = intercalate "|" $ zipWith padLabel header colWidths
      lines' = unlines $ map (flip padLine colWidths) lines
      sep    = replicate (sum colWidths) '-'
  return $ unlines [ids',"",lines']
    where 
      padLabel label width = fillCenter width label
      padLine line widths  = concat $ zipWith padValue line widths
      padValue value width = fillLeft (width + 4) value
showTuples _ = throwError "showTuples: parameter is not tuple"

showLine :: Value -> Eval [String]
showLine (Tuple _ xs) = mapM showValue' xs

colWidth :: [[String]] -> [Int] -> [Int]
colWidth [] widths        = widths
colWidth (line:ls) widths = colWidth ls newWidths
    where
      newWidths = zipWith maxWidth line widths
      maxWidth s width = max (length s) width

type ColWidth = Int

fillLeft :: ColWidth -> String -> String
fillLeft n s = s ++ replicate (n - length s) ' '

fillCenter :: ColWidth -> String -> String
fillCenter n s = pad ++ s ++ pad
    where 
      padWidth = (n - length s) `div` 2
      pad      = replicate padWidth ' '

modsfuns :: Eval Value
modsfuns = do
  Seq ms <- eval (RefE "modules")
  fs <- forM ms (evalApp' "functions")
  return $ concatValue fs

any_in :: Eq a => [a] -> [a] -> Bool
any_in xs ys = not (null (xs `intersect` ys))

all_in :: Eq a => [a] -> [a] -> Bool
all_in = subset

average :: [Int] -> [Int]
average [] = []
average xs = [round $ (fromIntegral $ sum xs) / (fromIntegral $ length xs)]
