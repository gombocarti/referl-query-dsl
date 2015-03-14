module SqRefact where

import Types (Id, UQuery(..), TUQuery(..), UF(..), Binop(..), UFun(..))
import Foreign.Erlang
import Data.List (union,nub)
import Text.Regex.Posix ((=~))
import Control.Monad.Reader
import Control.Monad (sequence)
import Data.Functor ((<$>))
import System.FilePath (takeFileName,takeDirectory)

import qualified Sq

data Value
    = File ErlType
    | Mod ErlType
    | Fun ErlType
    | Expr ErlType
    | Type ErlType
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

file_lib = "reflib_file"
module_lib = "reflib_module"
function_lib = "reflib_function"
query_lib = "reflib_query"
metrics = "refusr_metrics"

type ErlModule = String
type ErlFunction = String

newtype GraphPath = GPath ErlType deriving Show

data Database = Database 
    { runQuery  :: GraphPath -> IO ErlType
    , runQuery1 :: ErlType -> GraphPath -> IO ErlType
    , call :: ErlModule -> ErlFunction -> [ErlType] -> IO ErlType
    }
              
initErl :: String -> IO Database
initErl name = do 
  self <- createSelf name
  mbox <- createMBox self
  return $ database mbox

referl = "refactorerl"

database :: MBox -> Database
database mbox = 
    Database 
    { runQuery = \(GPath p) -> rpcCall mbox referl query_lib "exec" [p]
    , runQuery1 = \node (GPath p) -> rpcCall mbox referl query_lib "exec" [node, p]
    , call = rpcCall mbox referl
    }

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
eval UModules _env = do 
  ms <- queryDb UModules
  case ms of
    ErlNull    -> return $ Seq []
    ErlList xs -> return $ Seq . map Mod $ xs
eval UFiles _env = do
  fs <-queryDb UFiles
  case fs of
    ErlNull    -> return $ Seq []
    ErlList xs -> return $ Seq . map File $ xs
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


path :: UQuery -> Query GraphPath
path UModules = callDb module_lib "all" []
path UFiles   = callDb file_lib "all" []

-- path UModules = return . GPath $ ErlList [ErlTuple [ErlAtom "module",ErlTuple [ErlAtom "name", ErlAtom "/=", ErlList []]]]

--pathFun :: UFun -> Query GraphPath
pathFun UFunctions = callDb module_lib "locals" []
pathFun UCalls     = callDb function_lib "funcalls" []
pathFun UFile      = callDb module_lib "file" []
pathFun URecords   = callDb file_lib "records" []

{-
pathFun UFunctions = 
    return . GPath $ ErlList [ErlTuple 
                              [ErlAtom "func",
                               ErlTuple [ErlTuple [ErlAtom "opaque",ErlAtom "==",ErlAtom "false"],
                                         ErlAtom "and",
                                         ErlTuple [ErlAtom "type",ErlAtom "==",ErlAtom "regular"]]]]
-}

callDb' :: ErlModule -> ErlFunction -> [ErlType] -> Query ErlType
callDb' m f args = do db <- ask
                      liftIO $ call db m f args

callDb :: ErlModule -> ErlFunction -> [ErlType] -> Query GraphPath
callDb m f args = do db <- ask
                     p <- liftIO $ call db m f args
                     return $ GPath p

queryDb :: UQuery -> Query ErlType
queryDb q = do p <- path q
               db <- ask
               liftIO $ runQuery db p

queryDb1 :: ErlType -> UFun -> (ErlType -> Value) -> Query Value
queryDb1 x f g = do 
  p <- pathFun f
  db <- ask
  y <- liftIO $ runQuery1 db x p
  wrap g y

wrap :: (ErlType -> Value) -> ErlType -> Query Value
wrap _f ErlNull = return . Seq $ []
wrap f (ErlList xs) = return . Seq . map f $ xs
wrap f x = return . f $ x

unwrap = undefined

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
      Fun f  -> (String . fromErlang) <$> callDb' function_lib "name" [f]
      Mod m  -> queryDb1 m UModName (String . fromErlang)
      File f -> queryDb1 f UFileName (String . fromErlang)
evalApp UArity [Fun f] = (Int . fromErlang) <$> callDb' function_lib "arity" [f]
evalApp ULoc [arg] = do
    loc <- case arg of
             Fun f -> callDb' metrics "metric" [ErlTuple [ErlAtom "line_of_code",ErlAtom "function",f]]
             File f -> callDb' metrics "metric" [ErlTuple [ErlAtom "line_of_code",ErlAtom "file",f]]
    return . Int . fromErlang $ loc
evalApp UNot [Bool pred] = return . Bool . not $ pred
evalApp UNull [Seq xs] = return . Bool . null $ xs
evalApp UElem [a,Seq bs] = return . Bool $ a `elem` bs
evalApp USubset [Seq as,Seq bs] = return . Bool $ as `Sq.all_in` bs
evalApp UAnyIn [Seq as,Seq bs] = return . Bool $ as `Sq.any_in` bs
evalApp UUnion [Seq as,Seq bs] = return . Seq $ as `union` bs
evalApp UCalls [Fun f] = queryDb1 f UCalls Fun
evalApp UFunctions [Mod m] = queryDb1 m UFunctions Fun
evalApp URecords [File f] = queryDb1 f URecords Rec
evalApp UExported [Fun f] = do
  exported <-fromErlang <$> callDb' function_lib "is_exported" [f]
  return . Bool $ exported
evalApp UFile [Mod m] = queryDb1 m UFile File
evalApp UPath [File f] = (String . fromErlang) <$> callDb' file_lib "path" [f]
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

{-
evalApp UReturns [Fun f] = wrap . Sq.returns $ f
evalApp UOrigin [Expr expr] = wrap . Sq.origin $ expr
evalApp UFields [Rec r] = wrap . Sq.rfields $ r
evalApp UReferences [arg] = 
    case arg of
      Fun f      -> wrap . Sq.freferences $ f
      Rec r      -> wrap . Sq.rreferences $ r
      RecField f -> wrap . Sq.fieldReferences $ f
evalApp UExpressions [arg] =
    case arg of
      Fun f  -> wrap . Sq.expressions $ f
      Expr e -> wrap . Sq.expressions $ e
evalApp UMax [Seq xs] = Seq . Sq.max $ xs
evalApp UMin [Seq xs] = Seq . Sq.min $ xs
evalApp UAverage [xs] = wrap . Sq.average . unwrap $ xs
evalApp ULength [Chain c] = Int . length . getChain $ c
evalApp UDistinct [Chain c] = Chain $ fChain nub c
-}

getChain = undefined
fChain = undefined

showValue :: Value -> Query String
showValue (File f)   = do
  path <-  fromErlang <$> callDb' file_lib "path" [f]
  return . takeFileName $ path  
showValue (Mod m)    = fromErlang <$> callDb' module_lib "name" [m]
showValue (Fun f)    = fromErlang <$> callDb' function_lib "name" [f]
showValue (Int n)    = return . show $ n
showValue (String s) = return s
showValue (Bool b)   = return . show $ b
showValue (Seq xs)   = unlines <$> mapM showValue xs
