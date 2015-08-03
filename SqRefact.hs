module SqRefact where

import Prelude hiding (seq,mod)
import Types (Id, UQuery(..), UF(..))
import Foreign.Erlang
import Data.List (union,nub,groupBy,intercalate,partition)
import Text.Regex.Posix ((=~))
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error
import Data.Functor ((<$>))
import Data.Function (on)
import System.FilePath (takeFileName,takeDirectory)
import Data.String.Utils (strip)
import Sq (subset,Chain(..))

import Data.Char (isPrint)

import qualified Sq

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
    | ExprType Sq.ExprType
    | FunRecursivity Sq.DbFunctionType
    | String String
    | Int Int
    | Bool Bool
    | Unit
    | Path FilePath
    | Chain (Sq.Chain Value)
    | Seq [Value]
    | Grouped [(Value,Value)]
    | Tuple [UQuery] [Value]
    | FunDef [Id] [(Id,Value)] UQuery
    | Lambda Id UQuery
    | Curried Id [Value]
      deriving (Show,Eq)

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
type File = String

type Arg = (File,FilePosition)

getArg :: Query ErlType
getArg = do
  arg <- lift . lift $ ask
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

data Database = Database 
    { call :: ErlModule -> ErlFunction -> [ErlType] -> IO ErlType }
              
initErl :: String -> IO Database
initErl name = do 
  self <- createSelf name
  mbox <- createMBox self
  return $ database mbox

referl :: Node
referl = Short "refactorerl"

database :: MBox -> Database
database mbox = Database { call = rpcCall mbox referl }

type Env = [(Id, Value)]

type Query a = StateT Env (ReaderT Database (ReaderT (Maybe Arg) (ErrorT String IO))) a

runQuery :: Query a ->  Database -> Maybe Arg -> Env -> IO (Either String a)
runQuery q db arg env = runErrorT (runReaderT (runReaderT (evalStateT q env) db) arg)

eval :: UQuery -> Query Value
eval (UQuery q) = eval q
eval (UBind m (UF x body)) = do 
  Seq as <- eval m
  env <- get
  xs <- forM as (\a -> put ((x,a):env) >> eval body)
  put env
  return $ concatValue xs
eval (UReturn x) = do
  y <- eval x
  seq [y]
eval (UGuard p rest) = do
  Bool res <- eval p
  if res
  then eval rest
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
eval (UAppExpr "closureN" [n,fs,x]) = do
  Int n' <- eval n
  x' <- eval x
  Seq <$> closureNM n' f x'
    where f      = makeFun fs
eval (UAppExpr "lfp" [fs,x]) = do
  x' <- eval x
  Seq <$> lfpM f x'
    where f = makeFun fs
eval (UAppExpr "iteration" [n,fs,x]) = do
  Int n' <- eval n
  x' <- eval x
  Seq <$> iterationM n' f x'
    where f      = makeFun fs
eval (UAppExpr "chainInf" [fs,x]) = do 
  x' <- eval x
  Seq <$> chainInfM f x'
    where f = makeFun fs
eval (UAppExpr "chainN" [n,fs,x]) = do
  Int n' <- eval n
  x' <- eval x
  Seq <$> chainNM n' f x'
    where f = makeFun fs
-}
eval (URef "modules") = queryDb Mod (modpath "all")
eval (URef "files") = queryDb File (filepath "all")
eval (URef "atFunction") = do
  arg <- getArg
  f <- callDb' lib_args "function" [arg] (throwError "atFunction: no function at given position")
  return . Fun $ f
eval (URef "atFile") = do
  arg <- getArg
  f <- callDb' lib_args "file" [arg] (throwError "atFile: no file at given position")
  return . File $ f
eval (URef "atModule") = do
  arg <- getArg
  m <- callDb' lib_args "module" [arg] (throwError "atModule: no module at given position")
  return . Mod $ m
eval (URef "atExpr") = do
  arg <- getArg
  e <- callDb' lib_args "expression" [arg] (throwError "atExpression: no expression at given position")
  return . Expr $ e
eval (UTuple components) = do
  xs <- mapM eval components
  return $ Tuple components xs
eval (URef name) = readVar name
eval (ULambda x body) = return (Lambda x body)
eval (UWith defs q) = addFuns >> eval q
    where
      addFuns = forM defs addFun
      addFun (UFunDef f args body _) = modify ((f,FunDef args [] body):)
eval (UAppExpr (URef f) arg) = do
  arg' <- eval arg
  v <- maybeReadVar f
  case v of
    Just fundef -> evalApp fundef [arg']
    Nothing     -> g (section f) arg'
        where g f' a = evalApp f' [a]
eval (UAppExpr f arg) = do
    f' <- eval f
    arg' <- eval arg
    evalApp f' [arg']
eval (UStringLit s) = string s
eval (UNumLit i) = int $ i
eval (UBoolLit b) = bool b
eval (UExprTypeLit t) = return $ ExprType t
eval (UFunRecurLit fr) = return $ FunRecursivity fr
eval x = error (show x)

-- noob
erlError :: ErlType -> Bool
erlError (ErlTuple (ErlAtom x:_)) = x `elem` ["badrpc","reflib_error"]
erlError _ = False

callDb :: ErlModule -> ErlFunction -> [ErlType] -> Query ErlType
callDb mod fun args = do
  db <- ask
  liftIO $ call db mod fun args

callDb' :: ErlModule -> ErlFunction -> [ErlType] -> Query ErlType -> Query ErlType
callDb' mod fun args x = do
  db <- ask
  y <- liftIO $ call db mod fun args
  if erlError y
  then x
  else return y

queryDb :: (ErlType -> Value) -> GraphPath -> Query Value
queryDb f p = do
  p' <- evalPath p
  x <- callDb query_lib "exec" [p']
  wrap f x

evalPath :: GraphPath -> Query ErlType
evalPath (GPath mod fun) = callDb mod fun []
evalPath (GSeq xs) = do
  ps <- mapM evalPath xs
  callDb query_lib "seq" [ErlList ps]
evalPath (All xs) = do
  ps <- mapM evalPath xs
  callDb query_lib "all" [ErlList ps]

queryDb1 :: (ErlType -> Value) -> GraphPath -> ErlType -> Query Value
queryDb1 f p x = do 
  p' <- evalPath p
  y <- callDb query_lib "exec" [x,p']
  wrap f y

queryDb1' :: (ErlType -> Value) -> ErlModule-> ErlFunction -> ErlType -> Query Value
queryDb1' f mod fun x = do
  y <- callDb mod fun [x]
  wrap f y

propertyDb :: Erlang a => (a -> Value) -> ErlModule -> ErlFunction -> ErlType -> Query Value
propertyDb f mod fun arg = do 
  x <- callDb mod fun [arg]
  return . f . fromErlang $ x

wrap :: (ErlType -> Value) -> ErlType -> Query Value
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
                   
makeFun :: UQuery -> Value -> Query [Value]
makeFun (URef f) v = do Seq xs <- evalApp' f v
                        return xs
makeFun app@(UAppExpr _ _) v = do f <- eval app
                                  Seq xs <- evalApp f [v]
                                  return xs
makeFun (UFunComp args) v = do Seq xs <- foldM step (Seq [v]) (reverse args)
                               return xs
    where 
      step (Seq xs) (URef f) = concatValueM $ mapM (evalApp' f) xs
      step (Seq xs) app@(UAppExpr _ _) = do
        f <- eval app
        xs' <- mapM (\x -> evalApp f [x]) xs
        return $ concatValue xs'
      step (Seq _) _ = error "step: not function argument"
      step _       _ = error "step: not sequence argument"
makeFun _ _ = error "makeFun: not fuction argument"

lfpM :: (Value -> Query [Value]) -> Value -> Query [Value]
lfpM f x = loop [] [x]
    where 
      loop :: [Value] -> [Value] -> Query [Value]
      loop old curr = do
        let all = union old curr
        new <- mapM f curr
        let new' = concat new
        if new' `subset` all
        then return all
        else loop all new'            

closureNM :: Int -> (Value -> Query [Value]) -> Value -> Query [Value]
closureNM n f x = loop n [x]
    where 
      loop 0 xs = return xs
      loop m xs = do
        xs' <- concat <$> mapM f xs
        loop (m - 1) (union xs xs')  -- foldM

iterationM :: Int -> (Value -> Query [Value]) -> Value -> Query [Value]
iterationM n f x = loop n [x]
    where loop 0 xs = return xs
          loop m xs = do
            xs' <- concat <$> mapM f xs
            loop (m - 1) xs'

chainNM :: Int -> (Value -> Query [Value]) -> Value -> Query [Value]
chainNM n f x = do chains <- loop n [] [Incomplete [x]]
                   return $ map Chain chains
    where loop 0 finished unfinished = return (unfinished ++ finished)
          loop _ finished []         = return finished
          loop m finished unfinished = do
            new <- concat <$> mapM (cont f) unfinished
            let (unfinished',finished') = split new
            loop (m - 1) (finished' ++ finished) unfinished'
            

chainInfM :: (Value -> Query [Value]) -> Value -> Query [Value]
chainInfM f x = do finished <- loop [] [Incomplete [x]]
                   return $ map Chain finished
    where loop finished []         = return finished
          loop finished unfinished = do
            new <- concat <$> mapM (cont f) unfinished
            let (unfinished', finished') = split new
            loop (finished' ++ finished) unfinished'

cont :: Eq a => (a -> Query [a]) -> Chain a -> Query [Chain a]
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

concatValueM :: Query [Value] -> Query Value
concatValueM m = do
  xs <- m
  return $ concatValue xs

maybeReadVar :: Id  -> Query (Maybe Value)
maybeReadVar v = do 
  env <- get
  return $ lookup v env

readVar :: Id -> Query Value
readVar v = do
  env <- get
  case lookup v env of
    Just val -> return val
    Nothing  -> throwError ("undefined variable: " ++ v)

section :: Id -> Value
section f = Curried f []

evalApp' :: Id -> Value -> Query Value
evalApp' f arg = evalApp (section f) [arg]

evalApp :: Value -> [Value] -> Query Value
evalApp (Curried f [p1]) [p2] | isRelation = bool (evalRel p1 f p2)
                              where isRelation = f `elem` ["==","<",">","/="]
evalApp (Curried "name" []) [arg] = 
    case arg of
      Fun f      -> propertyDb String lib_function "name" f
      Mod m      -> propertyDb String lib_module "name" m
      File _     -> evalApp' "filename" arg
      Rec r      -> propertyDb String lib_record "name" r
      TypeExpr t -> propertyDb String lib_typeExp "name" t
      Type t     -> propertyDb String lib_type "name" t
-- TODO mit ad vissza a returntypes refactorerlben? (type vagy typeexpr?)
evalApp (Curried "includes" []) [File f] =
    queryDb1 File (filepath "includes") f
evalApp (Curried "arity" []) [Fun f] = propertyDb Int lib_function "arity" f
evalApp (Curried "loc" []) [arg]   = propertyDb Int metrics "metric" args
    where 
      args = ErlTuple [ErlAtom "line_of_code",ErlAtom typeTag,x]
      (typeTag, x) = case arg of
                       Fun f  -> ("function",f)
                       File f -> ("file",f)                      
evalApp (Curried "not" []) [Bool p] = bool . not $ p
evalApp (Curried "||" [Bool x]) [Bool y] = bool (x || y) 
evalApp (Curried "null" []) [Seq xs] = bool . null $ xs
evalApp (Curried "==" [x]) [y] = bool (x == y)
evalApp (Curried "/=" [x]) [y] = bool (x /= y)
evalApp (Curried ">" [x]) [y] = bool (x > y)
evalApp (Curried ">=" [x]) [y] = bool (x >= y)
evalApp (Curried "<" [x]) [y] = bool (x < y)
evalApp (Curried "<=" [x]) [y] = bool (x <= y)
evalApp (Curried "∈" [a]) [Seq bs] = bool $ a `elem` bs
evalApp (Curried "⊂" [Seq as]) [Seq bs] = bool $ as `Sq.all_in` bs
evalApp (Curried "any_in" [Seq as]) [Seq bs] = bool $ as `Sq.any_in` bs
evalApp (Curried "∪" [Seq as]) [Seq bs] = seq $ as `union` bs
evalApp (Curried "calls" []) arg = evalApp (Curried "callsP" [p]) arg
    where p = (Curried "const" [Bool True])
evalApp (Curried "callsP" [p]) [Fun f] = do 
  Seq funs <- queryDb1 Fun (funpath "funcalls") f
  funs' <- filterM pred funs
  seq funs'
    where 
      pred :: Value -> Query Bool
      pred fun = do Bool b <- evalApp p [fun]
                    return b
evalApp (Curried "functions" []) [Mod m] = queryDb1 Fun (modpath "locals") m
evalApp (Curried "records" []) [File f] = queryDb1 Rec (filepath "records") f
evalApp (Curried "exported" []) [Fun f] = propertyDb Bool lib_function "is_exported" f
evalApp (Curried "file" []) [Mod m] = queryDb1 File (modpath "file") m
evalApp (Curried "defmodule" []) [Fun f] = do
  Seq [m] <- queryDb1 Mod (funpath "module") f
  return m
evalApp (Curried "module" []) [File f] = queryDb1 Mod (filepath "module") f
evalApp (Curried "path" []) [File f] = propertyDb String lib_file "path" f
evalApp (Curried "dir" []) [f] = do 
  String path <- evalApp' "path" f
  string . takeDirectory $ path
evalApp (Curried "filename" []) [f] = do
  String path <- evalApp' "path" f
  string . takeFileName $ path
{-
evalApp UTypeOf [arg] = 
    case arg of 
      FunParam p -> callDb' 
      RecField f -> queryDb1 f URecFieldTypeOf Type
-}
-- evalApp UExprType [Expr e] = queryDb1 e UExprType ExprType

-- evalApp URecursivity [Fun f] = wrap . Sq.frecursive $ f

evalApp (Curried "returns" []) [Fun f] = queryDb1 Type path f
    where path = GSeq [ funpath "spec"
                      , specpath "returntypes"
                      ] -- todo: typexp -> namedtype konverzio
evalApp (Curried "origin" []) [Expr expr] = do
  es <- callDb dataflow "reach" args
  wrap Expr es
    where args = [ErlList [expr], ErlList [ErlAtom "back"]]
evalApp (Curried "fields" []) [Rec r] = queryDb1 RecField (recpath "fields") r
evalApp (Curried "references" []) [Fun f] =
    queryDb1' Expr lib_haskell "function_references" f
evalApp (Curried "references" []) [Rec f] = 
    queryDb1 Expr (recpath "references") f
evalApp (Curried "references" []) [RecField f] =
    queryDb1 Expr (recfieldpath "references") f
evalApp (Curried "expressions" []) [Fun f] = queryDb1 Expr path f
    where 
      path = GSeq [ funpath "definition"
                  , formpath "clauses"
                  , clausepath "exprs"
                  ]
evalApp (Curried "subexpressions" []) [Expr e] = 
    queryDb1' Expr lib_haskell "subexpressions" e
evalApp (Curried "index" []) [Expr e] =
    queryDb1' (Int . fromErlang) lib_sq "expr_index" e
evalApp (Curried "max" []) [Seq xs] = seq . Sq.max $ xs
evalApp (Curried "min" []) [Seq xs] = seq . Sq.min $ xs
evalApp (Curried "average" []) [Seq xs] = seq . map Int . Sq.average $ ns
    where ns = [n | Int n <- xs]

evalApp (Curried "length" []) [Chain c] = int . length . getChain $ c
evalApp (Curried "distinct" []) [Chain c] = chain $ fChain nub c
evalApp (Curried "distinct" []) [Seq xs] = seq $ nub xs
evalApp (Curried "const" [a]) [_] = return a
evalApp (Curried "groupBy" [f]) [Seq xs] = do
  ys <- forM xs (\x -> evalApp f [x])
  let zs = zip ys xs
  return $ Grouped (group zs)
    where 
      group ys = map merge (groupBy criteria ys)
      criteria = (==) `on` fst
      merge ys = (fst . head $ ys, Seq (map snd ys))
evalApp (Curried f args) [arg] = return $ Curried f (args ++ [arg])
evalApp (FunDef argNames ps body) params =
    let (defined,argNames') = splitAt (length params) argNames
        defined' = zip defined params ++ ps
    in
      case argNames' of
        [] -> do
          env <- get
          modify (defined' ++)
          result <- eval body
          put env
          return result
        _  -> return (FunDef argNames' defined' body)
evalApp _ _ = error "evalApp: not function"

string :: String -> Query Value
string = return . String

int :: Int -> Query Value
int = return . Int

bool :: Bool -> Query Value
bool = return . Bool

chain :: Sq.Chain Value -> Query Value
chain = return . Chain

seq :: [Value] -> Query Value
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

showValue' :: Value -> Query String
showValue' (Seq []) = return ""
showValue' (Seq xs@(Tuple _ _:_)) = showTuples xs
showValue' xs@(Seq _) = fromErlang <$> callDb lib_haskell "name" [xs']
    where xs' = toErlang xs
showValue' g@(Grouped _) = do
  xs <- fromErlang <$> callDb lib_haskell "name" [g']
  return . format $ xs
    where g'  = toErlang g
          format = foldr (\group acc -> formatGroup group ++ "\n" ++ acc) ""
          formatGroup (grouping,names) = grouping ++ "\n" ++ unlines ["   " ++ name | name <- names]
showValue' (Bool b) = return . show $ b
showValue' (Int n)  = return . show $ n
showValue' (String s) = return s
showValue' x        = showValue' (Seq [x])
          

showValue :: Value -> Query String
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

showTuples :: [Value] -> Query String
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

showLine :: Value -> Query [String]
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

modsfuns :: Query Value
modsfuns = do
  Seq ms <- eval (URef "modules")
  fs <- forM ms (evalApp' "functions")
  return $ concatValue fs
