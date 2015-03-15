module SqRefact where

import Prelude hiding (seq,mod)
import Types (Id, UQuery(..), TUQuery(..), UF(..), Binop(..), UFun(..))
import Foreign.Erlang
import Data.List (union,nub)
import Text.Regex.Posix ((=~))
import Control.Monad.Reader
import Data.Functor ((<$>))
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
      deriving (Eq,Show)

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

referl = "refactorerl"

database :: MBox -> Database
database mbox = Database { call = rpcCall mbox referl }

type Env = [(Id, Value)]

type Query a = ReaderT Database IO a

eval :: UQuery -> Env -> Query Value
eval (UBind m (UF x body)) env = do 
  Seq as <-  eval m env
  xs <- forM as (\a -> eval body ((x,a):env))
  return $ concatValue xs
eval (UVarExpr v) env = return $ readVar v env
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
eval (UAppExpr f args) env = do
  args' <- mapM (flip eval env) args
  evalApp f args'
eval UModules _env = queryDb Mod (modpath "all")
eval UFiles _env = queryDb File (filepath "all")
{-
eval UAtFunction _env = do
  f <- queryDb UAtFunction
  case f of
    ErlNull    -> return $ Seq []
    ErlTuple _ -> return $ Seq [Fun f]
eval UAtFile _env = do
  f <- queryDb UAtFile
  case f of
    ErlNull    -> return $ Seq []
    ErlTuple _ -> return $ Seq [Fun f]  
eval UAtModule _env = wrap Sq.atModule
eval UAtExpr _env = wrap Sq.atExpression
-}
eval (UReturn e) env = do 
  x <- eval e env
  return $ Seq [x]
eval (UStringLit s) _env = return $ String s
eval (UNumLit i) _env = return $ Int i
eval (UExprTypeLit t) _env = return $ ExprType t
eval (UFunRecurLit fr) _env = return $ FunRecursivity fr
eval (URelation rel p1 p2) env = do 
  p1' <- eval p1 env
  p2' <- eval p2 env
  return . Bool $ evalRel p1' rel p2'       
eval (UGuard pred) env = do
  Bool p <- eval pred env
  if p 
  then return $ Seq [Unit]
  else return $ Seq []


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

queryDb :: (ErlType -> Value) -> GraphPath -> Query Value
queryDb f p = do
  p' <- evalPath p
  x <- callDb query_lib "exec" [p']
  wrap f x

evalPath :: GraphPath -> Query ErlType
evalPath (GPath mod fun) = callDb mod fun []

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

readVar :: Id -> Env -> Value
readVar v env = case lookup v env of
                  Just x  -> x
                  Nothing -> error $ "undefined variable: " ++ v

evalApp :: UFun -> [Value] -> Query Value
evalApp UName [arg] = 
    case arg of
      Fun f      -> propertyDb String lib_function "name" f
      Mod m      -> propertyDb String lib_module "name" m
      File f     -> evalApp UFileName [File f]
      Rec r      -> propertyDb String lib_record "name" r
      TypeExpr t -> propertyDb String lib_typeExp "name" t
      Type t     -> propertyDb String lib_type "name" t
-- TODO mit ad vissza a returntypes refactorerlben?
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
                      ]
evalApp UOrigin [Expr expr] = do
  es <- callDb dataflow "reach" args
  wrap Expr es
    where args = [ErlList [expr], ErlList [ErlAtom "back"]]
evalApp UFields [Rec r] = queryDb1 RecField (recpath "fields") r
evalApp UReferences [Fun f] = queryDb1 Fun path f
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
evalApp UExpressions [Expr e] = error "unimplemented"
evalApp UMax [Seq xs] = seq . Sq.max $ xs
evalApp UMin [Seq xs] = seq . Sq.min $ xs
evalApp UAverage [Seq xs] = seq . map Int . Sq.average $ ns
    where ns = [n | Int n <- xs]
evalApp ULength [Chain c] = int . length . getChain $ c
evalApp UDistinct [Chain c] = chain $ fChain nub c

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
showValue (File f)   = do
  String path <- evalApp UFileName [File f]
  return path  
showValue (Mod m)    = do 
  String s <- evalApp UName [Mod m]
  return s
showValue (Fun f)    = do 
  String s <- evalApp UName [Fun f]
  return s
showValue (Rec r)    = do
  String s <- evalApp UName [Rec r]
  return s
showValue (Type t)   = do
  String s <- evalApp UName [Type t]
  return s
showValue (Expr e)   = do
  s <- callDb syntax "flat_text2" [e]
  return . strip . fromErlang $ s
showValue (Int n)    = return . show $ n
showValue (String s) = return s
showValue (Bool b)   = return . show $ b
showValue (Seq xs)   = unlines <$> mapM showValue xs
