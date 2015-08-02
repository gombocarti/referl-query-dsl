import Types
import Parser
--import HParser
import TypeCheck
--import qualified SqDeep as Sd
import SqRefact
-- import qualified Test
import Text.Parsec (runParserT)
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

run :: String -> Maybe Arg -> IO ()
run sq arg = do
  parseResult <- try (runParserT query Nothing "" sq)
  case parseResult of
    Right (Right tree) ->
        case runQCheck (check tree) funtypes of
          Right (q ::: _, _) -> 
              do
                db <- initErl self
                x <- runQuery (eval q >>= showValue') db arg initEnv
                case x of
                  Right s  -> putStrLn s
                  Left err -> putStrLn ("error: " ++ err)
          Left err -> putStrLn ("type error: " ++ err)
    Right (Left perror) -> putStrLn ("parse error: " ++ show perror)
    Left exc -> putStrLn ("i/o error: " ++ show (exc :: SomeException))
    where initEnv = [(f,Curried f []) | (f,_) <- funtypes]

{-
runchk :: String -> QParser UQuery -> TEnv ->  Either String TUQuery
runchk s parser env = case runParser parser Nothing "" s of
                        Right x -> check x env
                        Left err -> throwError . show $ err
-}

runmf :: IO ()
runmf = do
  db <- initErl self
  res <- runQuery (modsfuns >>= (showValue' . Seq . (flip flatten []))) db Nothing []
  --return res
  print res

runmf' :: Database -> IO (Either String Value)
runmf' db = do
  res <- runQuery (modsfuns) db Nothing []
  return res

main :: IO ()
main = do
  a <- getArgs
  case a of
    []                   -> time runmf >>= print
    [s]                  -> time (run s Nothing) >>= print
    [s,path,pos]
        | "~/" `isPrefixOf` path -> do 
           home <- getHomeDirectory
           run s (Just (home </> path,read pos))
        | otherwise            -> run s (Just (path,read pos))
    _                    -> do 
           name <- getProgName 
           putStrLn ("usage: " ++ name ++ " query")


time :: (IO a) -> IO (a,NominalDiffTime)
time m = do
  t1 <- getCurrentTime
  x <- m
  t2 <- getCurrentTime
  return (x,diffUTCTime t2 t1)
