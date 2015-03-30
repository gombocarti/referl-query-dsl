module SqRefact where

import Prelude hiding (seq,mod)
import Types (Id, UQuery(..), TUQuery(..), UF(..), Binop(..))
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
    | FunDef [Id] [(Id,Value)] UQuery
    | Section Id [Value]
      deriving (Show,Eq)

instance Ord Value where
    (Int a) <= (Int b) = a <= b
    (String s1) <= (String s2) = s1 <= s2
    (Bool a) <= (Bool b) = a <= b

lib_file = "reflib_file"
lib_module = "reflib_module"
lib_function = "reflib_function"
lib_record = "reflib_record"
lib_recordfield = "reflib_record_field"
lib_spec   = "reflib_spec"
lib_type   = "reflib_type"
lib_typeExp = "reflib_typexp"
lib_clause = "reflib_clause"
lib_form  = "reflib_form"
lib_expr = "reflib_expression"
lib_dynfun = "reflib_dynfun"
lib_args = "reflib_args"
lib_haskell = "reflib_haskell"
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
recfieldpath = GPath lib_recordfield
specpath   = GPath lib_spec
dynfunpath = GPath lib_dynfun

type FilePosition = Int
type File = String

type Arg = (File,FilePosition)

getArg :: Query ErlType
getArg = do
  Just (file,pos) <- lift . lift $ ask
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

type Query a = StateT Env (ReaderT Database (ReaderT (Maybe Arg) (ErrorT String IO))) a

eval :: UQuery -> Query Value
eval (UQuery q) = eval q
eval (UBind m (UF x body)) = do 
  Seq as <-  eval m
  env <- get
  xs <- forM as (\a -> put ((x,a):env) >> eval body)
  put env
  return $ concatValue xs
eval (UGroupBy f q) = do
  Seq xs <- eval q
  ys <- forM xs (evalApp' f)
  let zs = zip ys xs
  return $ Grouped (group zs)
    where 
      group xs = map merge (groupBy criteria xs)
      criteria = (==) `on` fst
      merge xs = (fst . head $ xs, Seq (map snd xs))
--eval (UVarExpr v) env = readVar v env
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
{-
--eval (UAppExpr UChainN [fs,v]) env = wrap $ Sq.chainInf f (eval v env)
--    where f = makeFun fs
-}
eval (URef name) = readVar name
eval (UWith defs q) = addFuns >> eval q
    where
      addFuns = forM defs addFun
      addFun (UFunDef f args body _) = modify ((f,FunDef args [] body):)
eval (UAppExpr f args) = do
  args' <- mapM eval args
  v <- maybeReadVar f
  case v of
    Just fundef -> evalApp fundef args'
    Nothing     -> foldM g (section f) args'
        where g f' a = evalApp f' [a]
eval UModules = queryDb Mod (modpath "all")
eval UFiles = queryDb File (filepath "all")
eval UAtFunction = do
  arg <- getArg
  f <- callDb' lib_args "function" [arg] (throwError "atFunction: no function at given position")
  return . Fun $ f
eval UAtFile = do
  arg <- getArg
  f <- callDb' lib_args "file" [arg] (throwError "atFile: no file at given position")
  return . File $ f
eval UAtModule = do
  arg <- getArg
  m <- callDb' lib_args "module" [arg] (throwError "atModule: no module at given position")
  return . Mod $ m
eval UAtExpr = do
  arg <- getArg
  e <- callDb' lib_args "expression" [arg] (throwError "atExpression: no expression at given position")
  return . Expr $ e
eval (UReturn e) = do 
  x <- eval e
  seq [x]
eval (UTuple components) = do
  xs <- mapM eval components
  return $ Tuple components xs
eval (UStringLit s) = string s
eval (UNumLit i) = int i
eval (UBoolLit b) = bool b
eval (UExprTypeLit t) = return $ ExprType t
eval (UFunRecurLit fr) = return $ FunRecursivity fr
eval (URelation rel p1 p2) = do 
  p1' <- eval p1
  p2' <- eval p2
  bool $ evalRel p1' rel p2'       
eval (UGuard pred) = do
  Bool p <- eval pred
  if p 
  then seq [Unit]
  else seq []

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

chainInfM :: (Value -> Query [Value]) -> Value -> Query [Value]
chainInfM f x = do finished <- loop [] [Incomplete [x]]
                   return $ map Chain finished
    where loop finished []         = return finished
          loop finished unfinished = do
            new <- concat <$> mapM cont unfinished
            let (unfinished', finished') = split new
            loop (finished' ++ finished) unfinished'

          cont (Incomplete chain@(z:_)) = do
            xs <- f z
            case xs of
              [] -> return [Complete chain]
              ys -> return [classify y chain | y <- ys]

          classify y chain | y `elem` chain = Recursive chain
                           | otherwise      = Incomplete (y:chain)

          split chains = partition isInComplete chains 

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
section f = Section f []

evalApp' :: Id -> Value -> Query Value
evalApp' f arg = evalApp (section f) [arg]

evalApp :: Value -> [Value] -> Query Value
evalApp (Section "name" []) [arg] = 
    case arg of
      Fun f      -> propertyDb String lib_function "name" f
      Mod m      -> propertyDb String lib_module "name" m
      File _     -> evalApp' "filename" arg
      Rec r      -> propertyDb String lib_record "name" r
      TypeExpr t -> propertyDb String lib_typeExp "name" t
      Type t     -> propertyDb String lib_type "name" t
-- TODO mit ad vissza a returntypes refactorerlben? (type vagy typeexpr?)
evalApp (Section "arity" []) [Fun f] = propertyDb Int lib_function "arity" f
evalApp (Section "loc" []) [arg]   = propertyDb Int metrics "metric" args
    where 
      args = ErlTuple [ErlAtom "line_of_code",ErlAtom typeTag,x]
      (typeTag, x) = case arg of
                       Fun f  -> ("function",f)
                       File f -> ("file",f)                      
evalApp (Section "not" []) [Bool pred] = bool . not $ pred
evalApp (Section "null" []) [Seq xs] = bool . null $ xs
evalApp (Section "∈" [a]) [Seq bs] = bool $ a `elem` bs
evalApp (Section "⊂" [Seq as]) [Seq bs] = bool $ as `Sq.all_in` bs
evalApp (Section "any_in" [Seq as]) [Seq bs] = bool $ as `Sq.any_in` bs
evalApp (Section "∪" [Seq as]) [Seq bs] = seq $ as `union` bs
evalApp (Section "calls" []) arg = evalApp (Section "callsP" [p]) arg
    where p = (Section "const" [Bool True])
evalApp (Section "callsP" [p]) [Fun f] = do 
  Seq funs <- queryDb1 Fun (funpath "funcalls") f
  funs' <- filterM pred funs
  seq funs'
    where 
      pred :: Value -> Query Bool
      pred fun = do Bool b <- evalApp p [fun]
                    return b
evalApp (Section "functions" []) [Mod m] = queryDb1 Fun (modpath "locals") m
evalApp (Section "records" []) [File f] = queryDb1 Rec (filepath "records") f
evalApp (Section "exported" []) [Fun f] = propertyDb Bool lib_function "is_exported" f
evalApp (Section "file" []) [Mod m] = queryDb1 File (modpath "file") m
evalApp (Section "defmodule" []) [Fun f] = queryDb1 Mod (funpath "module") f
evalApp (Section "module" []) [File f] = queryDb1 Mod (filepath "module") f
evalApp (Section "path" []) [File f] = propertyDb String lib_file "path" f
evalApp (Section "dir" []) [f] = do 
  String path <- evalApp' "path" f
  return . String . takeDirectory $ path
evalApp (Section "filename" []) [f] = do
  String path <- evalApp' "path" f
  return . String . takeFileName $ path
{-
evalApp UTypeOf [arg] = 
    case arg of 
      FunParam p -> callDb' 
      RecField f -> queryDb1 f URecFieldTypeOf Type
-}
-- evalApp UExprType [Expr e] = queryDb1 e UExprType ExprType

-- evalApp URecursivity [Fun f] = wrap . Sq.frecursive $ f

evalApp (Section "returns" []) [Fun f] = queryDb1 Type path f
    where path = GSeq [ funpath "spec"
                      , specpath "returntypes"
                      ] -- todo: typexp -> namedtype konverzio
evalApp (Section "origin" []) [Expr expr] = do
  es <- callDb dataflow "reach" args
  wrap Expr es
    where args = [ErlList [expr], ErlList [ErlAtom "back"]]
evalApp (Section "fields" []) [Rec r] = queryDb1 RecField (recpath "fields") r
evalApp (Section "references" []) [Fun f] =
    queryDb1' Expr lib_haskell "function_references" f
evalApp (Section "references" []) [Rec f] = 
    queryDb1 Expr (recpath "references") f
evalApp (Section "references" []) [RecField f] =
    queryDb1 Expr (recfieldpath "references") f
evalApp (Section "expressions" []) [Fun f] = queryDb1 Expr path f
    where 
      path = GSeq [ funpath "definition"
                  , formpath "clauses"
                  , clausepath "exprs"
                  ]
evalApp (Section "expressions" []) [Expr e] = 
    queryDb1' Expr lib_haskell "subexpressions" e
evalApp (Section "max" []) [Seq xs] = seq . Sq.max $ xs
evalApp (Section "min" []) [Seq xs] = seq . Sq.min $ xs
evalApp (Section "average" []) [Seq xs] = seq . map Int . Sq.average $ ns
    where ns = [n | Int n <- xs]

evalApp (Section "length" []) [Chain c] = int . length . getChain $ c
evalApp (Section "distinct" []) [Chain c] = chain $ fChain nub c
evalApp (Section "const" [a]) [_] = return a
evalApp (Section f args) [arg] = return $ Section f (args ++ [arg])
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
fChain f (Sq.Complete xs)   = Sq.Complete $ f xs
fChain f (Sq.Recursive xs)  = Sq.Recursive $ f xs

getChain :: Sq.Chain a -> [a]
getChain (Sq.Incomplete xs) = xs
getChain (Sq.Complete xs)   = xs
getChain (Sq.Recursive xs)  = xs

showValue :: Value -> Query String
showValue f@(File _)   = do
  String name <- evalApp' "filename" f
  return name  
showValue m@(Mod _)    = do 
  String s <- evalApp' "name" m
  return s
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
  return $ unwords s ++ "..."
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
