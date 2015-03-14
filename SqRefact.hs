module SqRefact where

import Types (Id, UQuery(..), TUQuery(..), UF(..), Binop(..), UFun(..))
import Foreign.Erlang
import Data.List (union,nub)
import Text.Regex.Posix ((=~))
import Control.Monad.Reader
import Control.Monad (sequence)
import Data.Functor ((<$>))

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

module_lib = "reflib_module"
function_lib = "reflib_function"
query_lib = "reflib_query"

type ErlModule = String
type ErlFunction = String

newtype GraphPath = GPath ErlType deriving Show

data Database = Database 
    { runQuery  :: GraphPath -> IO ErlType
    , runQuery1 :: ErlType -> GraphPath -> IO ErlType
    , call :: ErlModule -> ErlFunction -> [ErlType] -> IO ErlType
    }
              
initErl :: IO (Database,Self)
initErl = do 
  self <- createSelf "haskell@localhost"
  mbox <- createMBox self
  return $ (database mbox,self)

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
  p <- path UModules
  ms <- queryDb p
  case ms of
    ErlNull    -> return $ Seq []
    ErlList xs -> return $ Seq . map Mod $ xs
eval UFiles _env = do
  p <- path UFiles
  fs <-queryDb p
  case fs of
    ErlNull    -> return $ Seq []
    ErlList xs -> return $ Seq . map File $ xs
eval UAtFunction _env = do
  p <- path UAtFunction
  f <- queryDb p
  case f of
    ErlNull    -> return $ Seq []
    ErlTuple _ -> return $ Seq [Fun f]
eval UAtFile _env = wrap Sq.atFile
eval UAtModule _env = wrap Sq.atModule
eval UAtExpr _env = wrap Sq.atExpression
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

-- path UModules = return . GPath $ ErlList [ErlTuple [ErlAtom "module",ErlTuple [ErlAtom "name", ErlAtom "/=", ErlList []]]]

--pathFun :: UFun -> Query GraphPath
pathFun UFunctions = callDb module_lib "locals" []

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

queryDb :: GraphPath -> Query ErlType
queryDb p = do db <- ask
               liftIO $ runQuery db p

queryDb1 :: ErlType -> GraphPath -> Query ErlType
queryDb1 x p = do db <- ask
                  liftIO $ runQuery1 db x p


wrap = undefined

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
{-
evalApp UName [arg] = wrap . Sq.name $ arg
evalApp UArity [Fun f] = wrap . Sq.arity $ f
evalApp ULoc [arg] = case arg of
                       Fun f -> wrap . Sq.loc $ f
                       File f -> wrap . Sq.loc $ f
                       Mod m -> wrap . Sq.loc $ m
evalApp UNot [Bool pred] = wrap $ not pred
evalApp UNull [Seq xs] = wrap $ null xs
evalApp UElem [a,Seq bs] = wrap $ a `elem` bs
evalApp USubset [Seq as,Seq bs] = wrap $ as `Sq.all_in` bs
evalApp UAnyIn [Seq as,Seq bs] = wrap $ as `Sq.any_in` bs
evalApp UUnion [Seq as,Seq bs] = Seq $ as `union` bs
evalApp UCalls [Fun f] = wrap $  Sq.fcalls f
-}
evalApp UFunctions [Mod m] = do p <- pathFun UFunctions
                                fs <- queryDb1 m p
                                case fs of
                                  ErlNull    -> return $ Seq []
                                  ErlList xs -> return . Seq . map Fun $ xs
                                
{-
evalApp URecords [File f] = wrap . Sq.frecords $ f
evalApp UExported [Fun f] = wrap . Sq.fexported $ f
evalApp UFile [Mod m] = wrap . Sq.mfile $ m
evalApp UPath [File f] = wrap . Sq.fpath $ f
evalApp UDir [File f] = wrap . Sq.dir $ f
evalApp UFileName [File f] = wrap . Sq.filename $ f
evalApp UTypeOf [arg] = case arg of 
                          FunParam p -> wrap . Sq.fptype $ p
                          RecField f -> wrap . Sq.fieldType $ f
evalApp UExprType [Expr e] = wrap . Sq.etype $ e
evalApp URecursivity [Fun f] = wrap . Sq.frecursive $ f
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

showValue :: Value -> Query String
showValue (Mod m) = fromErlang <$> callDb' module_lib "name" [m]
showValue (Fun f) = fromErlang <$> callDb' function_lib "name" [f]
showValue (Seq xs) = unlines <$> mapM showValue xs
