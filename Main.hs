import Types
import Parser
--import HParser
import TypeCheck
--import qualified SqDeep as Sd
import SqRefact
-- import qualified Test
import System.Environment (getArgs, getProgName)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Control.Exception (try, SomeException)
import Data.List (isPrefixOf)

import Data.Time.Clock

self :: String
self = "haskell@localhost"
{-
run :: String -> Maybe Arg -> IO ()
run sq arg = do
  let tree = transform . parse . lexer $ sq
  case runQCheck (check tree) funtypes of
    Right (q ::: _, _)-> 
        do
          db <- initErl self
          x <- runQuery (eval q >>= showValue' . Seq . flip flatten []) db arg
          case x of
            Right s  -> putStrLn s
            Left err -> putStrLn ("error: " ++ err)
    Left err -> putStrLn ("type error: " ++ err)
  -}     

run ::  String -> Maybe Arg -> Database -> IO ()
run sq arg db = do
  parseResult <- try (parseQuery sq)
  case parseResult of
    Right (Right tree) ->
        case runQCheck (check tree) funtypes of
          Right (q ::: _, _) -> 
              do
                x <- runEval (eval q >>= showValue') db arg initEnv
                putStrLn x
--                case x of
--                  Right s  -> putStrLn s
--                  Left err -> putStrLn ("error: " ++ err)
          Left err -> putStrLn ("type error: " ++ err)
    Right (Left perror) -> putStrLn ("parse error: " ++ show perror)
    Left exc -> putStrLn ("i/o error: " ++ show (exc :: SomeException))
    where initEnv = [(f,curried f) | (f,_) <- funtypes]

{-
runchk :: String -> QParser UQuery -> TEnv ->  Either String TUQuery
runchk s parser env = case runParser parser Nothing "" s of
                        Right x -> check x env
                        Left err -> throwError . show $ err
-}
{-
runmf :: IO ()
runmf = do
  db <- initErl self
  res <- runQuery (modsfuns >>= (showValue' . Seq . (flip flatten []))) db Nothing []
  --return res
  print res
-}
{-
runmf' :: Database -> IO (Either String Value)
runmf' db = do
  res <- runQuery (modsfuns) db Nothing []
  return res
-}

main :: IO ()
main = do
  db <- initErl self
  a <- getArgs
  case a of
    []                   -> run modsfuns Nothing db
    [s]  
        | s == "benchmark" -> benchmark db
        | otherwise -> (run s Nothing db) >>= print
    [s,path,pos]
        | "~/" `isPrefixOf` path -> do 
           home <- getHomeDirectory
           run s (Just (home </> path,read pos)) db
        | otherwise            -> run s (Just (path,read pos)) db
    _                    -> do 
           name <- getProgName 
           putStrLn ("usage: " ++ name ++ " query")
  where
    modsfuns = "{f | m <- modules, f <- functions m}"


time :: (IO a) -> IO (a,NominalDiffTime)
time m = do
  t1 <- getCurrentTime
  x <- m
  t2 <- getCurrentTime
  return (x,diffUTCTime t2 t1)


queries :: [String]
queries = ["{ref | ref <- references (record atField), not (null {e | e <- subexpressions ref, exprType e == Record_field, exprValue e == \"state\"}), exprType ref == Record_update}"
          , "{ref | m <- modules, name m == \"korte\", f <- functions m, ref <- references f, not (null {s2 | s1 <- subexpressions ref, index s1 == 1, exprType s1 == Tuple, s2 <- subexpressions s1, index s2 == 1, exprValue s2 == \"1\"})}"
          , "{ref | m <- modules, name m == \"korte\", f <- functions m, ref <- references f, not (null {s2 | s1 <- subexpressions ref, index s1 == 1 || index s1 == 2, exprType s1 == Tuple, s2 <- subexpressions s1, index s2 == 1, exprValue s2 == \"1\"})}"
          , "{ref | m <- modules, name m == \"korte\", f <- functions m, ref <- references f, not (null {s | p1 <- exprParams ref, index p1 == 2, p2 <- exprParams p1, index p2 == 2, o <- origin p2, s <- subexpressions o, exprValue s == \"snmp\"}) }"]

benchmark :: Database -> IO ()
benchmark db = do x <- time $ mapM_ (\s -> Main.run s (Just ("/home/r2r/erlang/korte.erl",244)) db) queries
                  print x
