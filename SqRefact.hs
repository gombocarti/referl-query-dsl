module SqRefact where

import Prelude hiding (seq,mod)
import Types (Id, UQuery(..), TUQuery(..), UF(..), Binop(..), UFun(..))
import Foreign.Erlang
import Data.List (union,nub,groupBy,intercalate)
import Text.Regex.Posix ((=~))
import Control.Monad.Reader
import Control.Monad.Error
import Data.Functor ((<$>))
import Data.Function (on)
import System.FilePath (takeFileName,takeDirectory)
import Data.String.Utils (strip)

import qualified Sq

data Value
    = File ErlType
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
    | FunDef UQuery
      deriving (Show,Eq)

instance Ord Value where
    (Int a) <= (Int b) = a <= b
    (String s1) <= (String s2) = s1 <= s2
    (Bool a) <= (Bool b) = a <= b

lib_file = "reflib_file"
lib_module = "reflib_module"
lib_function = "reflib_function"
lib_record = "reflib_record"
lib_spec   = "reflib_spec"
lib_type   = "reflib_type"
lib_typeExp = "reflib_typexp"
lib_clause = "reflib_clause"
lib_form  = "reflib_form"
lib_expr = "reflib_expression"
lib_dynfun = "reflib_dynfun"
lib_args = "reflib_args"
dataflow  = "refanal_dataflow"
query_lib = "reflib_query"
metrics = "refusr_metrics"
syntax  = "refcore_syntax"

filepath   = GPath lib_file
modpath    = GPath lib_module
funpath    = GPath lib_function
clausepath = GPath lib_clause
formpath   = GPath lib_form
recpath    = GPath lib_record
specpath   = GPath lib_spec
dynfunpath = GPath lib_dynfun

type FilePosition = Int
type File = String

type Arg = (File,FilePosition)

getArg :: Query ErlType
getArg = do
  Just (file,pos) <- lift ask
  return $ ErlList [ erlPair (ErlAtom "ask_missing") (ErlAtom "false")
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

referl :: String
referl = "refactorerl"

database :: MBox -> Database
database mbox = Database { call = rpcCall mbox referl }

type Env = [(Id, Value)]

type Query a = ReaderT Database (ReaderT (Maybe Arg) (ErrorT String IO)) a

eval :: UQuery -> Env -> Query Value
eval (UBind m (UF x body)) env = do 
  Seq as <-  eval m env
  xs <- forM as (\a -> eval body ((x,a):env))
  return $ concatValue xs
eval (UGroupBy f q) env = do
  Seq xs <- eval q env
  ys <- forM xs (\x -> evalApp f [x])
  let zs = zip ys xs
  return $ Grouped (group zs)
    where 
      group xs = map merge (groupBy criteria xs)
      criteria = (==) `on` fst
      merge xs = (fst . head $ xs, Seq (map snd xs))
eval (UVarExpr v) env = readVar v env
{-
eval (UAppExpr UClosureN [n,fs,v]) env = Seq $ Sq.closureN n' f (eval v env)
    where f      = makeFun fs
          Int n' = eval n env
eval (UAppExpr ULfp [fs,v]) env = Seq $ Sq.lfp f (eval v env)
    where f = makeFun fs
eval (UAppExpr UIteration [n,fs,v]) env = Seq $ Sq.iteration n' f (eval v env)
    where f      = makeFun fs
          Int n' = eval n env
eval (UAppExpr UChainInf [fs,v]) env = wrap $ Sq.chainInf f (eval v env)
    where f = makeFun fs
--eval (UAppExpr UChainN [fs,v]) env = wrap $ Sq.chainInf f (eval v env)
--    where f = makeFun fs
-}
eval (UFunDef _ [] body) env = eval body env     
eval (UWith defs q) env = eval q (funs ++ env)
    where
      funs = [(f, FunDef def) | def@(UFunDef f _ _) <- defs]
eval (UAppExpr (UFName f) args) env = do
  args' <- mapM (flip eval env) args
  FunDef funDef <- readVar f env
  evalFunDef funDef args' env
eval (UAppExpr f args) env = do
  args' <- mapM (flip eval env) args
  evalApp f args'
eval UModules _env = queryDb Mod (modpath "all")
eval UFiles _env = queryDb File (filepath "all")
eval UAtFunction _env = do
  arg <- getArg
  f <- callDb' lib_args "function" [arg] (throwError "atFunction: no function at given position")
  return . Fun $ f
eval UAtFile _env = do
  arg <- getArg
  f <- callDb' lib_args "file" [arg] (throwError "atFile: no file at given position")
  return . File $ f
eval UAtModule _env = do
  arg <- getArg
  m <- callDb' lib_args "module" [arg] (throwError "atModule: no module at given position")
  return . Mod $ m
eval UAtExpr _env = do
  arg <- getArg
  e <- callDb' lib_args "expression" [arg] (throwError "atExpression: no expression at given position")
  return . Expr $ e
eval (UReturn e) env = do 
  x <- eval e env
  seq [x]
eval (UTuple components) env = do
  xs <- mapM (flip eval env) components
  return $ Tuple components xs
eval (UStringLit s) _env = return $ String s
eval (UNumLit i) _env = return $ Int i
eval (UExprTypeLit t) _env = return $ ExprType t
eval (UFunRecurLit fr) _env = return $ FunRecursivity fr
eval (URelation rel p1 p2) env = do 
  p1' <- eval p1 env
  p2' <- eval p2 env
  bool $ evalRel p1' rel p2'       
eval (UGuard pred) env = do
  Bool p <- eval pred env
  if p 
  then seq [Unit]
  else seq []

-- noob
erlError :: ErlType -> Bool
erlError (ErlTuple (ErlAtom x:_)) = x `elem` ["badrpc","reflib_error"]
erlError _ = False


-- path UModules = return . GPath $ ErlList [ErlTuple [ErlAtom "module",ErlTuple [ErlAtom "name", ErlAtom "/=", ErlList []]]]
{-
pathFun UFunctions = 
    return . GPath $ ErlList [ErlTuple 
                              [ErlAtom "func",
                               ErlTuple [ErlTuple [ErlAtom "opaque",ErlAtom "==",ErlAtom "false"],
                                         ErlAtom "and",
                                         ErlTuple [ErlAtom "type",ErlAtom "==",ErlAtom "regular"]]]]
-}

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

propertyDb :: Erlang a => (a -> Value) -> ErlModule -> ErlFunction -> ErlType -> Query Value
propertyDb f mod fun arg = do 
  x <- callDb mod fun [arg]
  return . f . fromErlang $ x

wrap :: (ErlType -> Value) -> ErlType -> Query Value
wrap _f ErlNull     = return . Seq $ []
wrap f (ErlList xs) = return . Seq . map f $ xs
wrap f x            = return . f $ x

evalRel :: Value -> Binop -> Value -> Bool
evalRel a Eq  b = a == b
evalRel a NEq b = a /= b
evalRel a Gt  b = a >  b
evalRel a Gte b = a >= b
evalRel a Lt  b = a <  b
evalRel a Lte b = a <= b
evalRel a Regexp b = s =~ regex
    where String s     = a
          String regex = b
                   
{-
-- works for all function of type a -> [a] for some a:
makeFun (UFunRef f) = \v -> let Seq xs = evalApp f [v] in xs
makeFun (UFunComp args) = \v -> let Seq xs = foldr step (Seq [v]) args in xs
    where
      step f (Seq val) = concatValue $ map (\arg -> evalApp f [arg]) val
-}

concatValue :: [Value] -> Value
concatValue vals = Seq $ foldr step [] vals
    where step (Seq xs) acc = xs ++ acc

readVar :: Id -> Env -> Query Value
readVar v env = case lookup v env of
                  Just x  -> return x
                  Nothing -> throwError $ "undefined variable: " ++ v

evalApp :: UFun -> [Value] -> Query Value
evalApp UName [arg] = 
    case arg of
      Fun f      -> propertyDb String lib_function "name" f
      Mod m      -> propertyDb String lib_module "name" m
      File f     -> evalApp UFileName [File f]
      Rec r      -> propertyDb String lib_record "name" r
      TypeExpr t -> propertyDb String lib_typeExp "name" t
      Type t     -> propertyDb String lib_type "name" t
-- TODO mit ad vissza a returntypes refactorerlben? (type vagy typeexpr?)
evalApp UArity [Fun f] = propertyDb Int lib_function "arity" f
evalApp ULoc [arg]   = propertyDb Int metrics "metric" args
    where 
      args = ErlTuple [ErlAtom "line_of_code",ErlAtom typeTag,x]
      (typeTag, x) = case arg of
                       Fun f  -> ("function",f)
                       File f -> ("file",f)                      
evalApp UNot [Bool pred] = bool . not $ pred
evalApp UNull [Seq xs] = bool . null $ xs
evalApp UElem [a,Seq bs] = bool $ a `elem` bs
evalApp USubset [Seq as,Seq bs] = bool $ as `Sq.all_in` bs
evalApp UAnyIn [Seq as,Seq bs] = bool $ as `Sq.any_in` bs
evalApp UUnion [Seq as,Seq bs] = seq $ as `union` bs
evalApp UCalls [Fun f] = queryDb1 Fun (funpath "funcalls") f
evalApp UFunctions [Mod m] = queryDb1 Fun (modpath "locals") m
evalApp URecords [File f] = queryDb1 Rec (filepath "records") f
evalApp UExported [Fun f] = propertyDb Bool lib_function "is_exported" f
evalApp UFile [Mod m] = queryDb1 File (modpath "file") m
evalApp UDefModule [Fun f] = queryDb1 Mod (funpath "module") f
evalApp UModule [File f] = queryDb1 Mod (filepath "module") f
evalApp UPath [File f] = propertyDb String lib_file "path" f
evalApp UDir f = do 
  String path <- evalApp UPath f
  return . String . takeDirectory $ path
evalApp UFileName f = do
  String path <- evalApp UPath f
  return . String . takeFileName $ path
{-
evalApp UTypeOf [arg] = 
    case arg of 
      FunParam p -> callDb' 
      RecField f -> queryDb1 f URecFieldTypeOf Type
-}
-- evalApp UExprType [Expr e] = queryDb1 e UExprType ExprType

-- evalApp URecursivity [Fun f] = wrap . Sq.frecursive $ f

evalApp UReturns [Fun f] = queryDb1 Type path f
    where path = GSeq [ funpath "spec"
                      , specpath "returntypes"
                      ] -- todo: typexp -> namedtype konverzio
evalApp UOrigin [Expr expr] = do
  es <- callDb dataflow "reach" args
  wrap Expr es
    where args = [ErlList [expr], ErlList [ErlAtom "back"]]
evalApp UFields [Rec r] = queryDb1 RecField (recpath "fields") r
evalApp UReferences [Fun f] = queryDb1 Expr path f
    where 
      path = All [ funpath "applications"
                 , funpath "implicits"
                 , funpath "impexps"
                 , dynfunpath "dynfun_call"
                 ]
{-
evalApp UReferences [arg] = 
    case arg of
      Fun f      -> wrap . Sq.freferences $ f
      Rec r      -> wrap . Sq.rreferences $ r
      RecField f -> wrap . Sq.fieldReferences $ f
-}
evalApp UExpressions [Fun f] = queryDb1 Expr path f
    where 
      path = GSeq [ funpath "definition"
                  , formpath "clauses"
                  , clausepath "exprs"
                  ]
evalApp UExpressions [Expr e] = throwError "unimplemented"
evalApp UMax [Seq xs] = seq . Sq.max $ xs
evalApp UMin [Seq xs] = seq . Sq.min $ xs
evalApp UAverage [Seq xs] = seq . map Int . Sq.average $ ns
    where ns = [n | Int n <- xs]
evalApp ULength [Chain c] = int . length . getChain $ c
evalApp UDistinct [Chain c] = chain $ fChain nub c

evalFunDef (UFunDef _ argNames body) params env =
    eval body (zip argNames params ++ env)


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
fChain f (Sq.Complete xs)   = Sq.Complete $ f xs
fChain f (Sq.Recursive xs)  = Sq.Recursive $ f xs

getChain :: Sq.Chain a -> [a]
getChain (Sq.Incomplete xs) = xs
getChain (Sq.Complete xs)   = xs
getChain (Sq.Recursive xs)  = xs

showValue :: Value -> Query String
showValue f@(File _)   = do
  String name <- evalApp UFileName [f]
  return name  
showValue m@(Mod _)    = do 
  String s <- evalApp UName [m]
  return s
showValue f@(Fun _)    = do 
  String name <- evalApp UName [f]
  Int arity <- evalApp UArity [f]
  return $ name ++ "/" ++ show arity
showValue r@(Rec _)    = do
  String s <- evalApp UName [r]
  return s
showValue t@(Type _)   = do
  String s <- evalApp UName [t]
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

showTuples :: [Value] -> Query String
showTuples ts@((Tuple components _):_) = do
  let header = map show components
  lines <- mapM showLine ts
  let colWidths = colWidth lines (replicate (length header) 0)
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
showLine (Tuple _ xs) = mapM showValue xs

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
