import Types
--import Parser
import HParser
import TypeCheck
--import qualified SqDeep as Sd
import SqRefact
-- import qualified Test
import Text.Parsec (runParserT)
import Control.Monad (when)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import System.Environment (getArgs, getProgName)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Control.Exception (try, SomeException)

import Data.Time.Clock

runQuery :: Query a ->  Database -> Maybe Arg -> IO (Either String a)
runQuery q db arg = runErrorT (runReaderT (runReaderT (evalStateT q []) db) arg)

self :: String
self = "haskell@localhost"

run :: String -> Maybe Arg -> IO ()
run sq arg = do
  let tree = transform . parse . lexer $ sq
  case runQCheck (check tree) funtypes of
    Right (q ::: _, _)-> 
        do
          db <- initErl self
          x <- runQuery (eval q >>= showValue') db arg
          case x of
            Right s  -> print s
            Left err -> putStrLn ("error: " ++ err)
    Left err -> putStrLn ("type error: " ++ err)
       
{-                    
run :: String -> Maybe Arg -> IO ()
run sq arg = do
  parseResult <- try (runParserT query Nothing "" sq)
  case parseResult of
    Right (Right tree) ->
        case runQCheck (check tree) funtypes of
          Right ((q ::: _, _), warnings)-> 
              do
                when (warnings /= []) 
                         (putStrLn ("warning:\n" ++ unlines warnings))
                db <- initErl self
                x <- runQuery (eval q) db arg
                case x of
                  Right s  -> print s
                  Left err -> putStrLn ("error: " ++ err)
          Left err -> putStrLn ("type error: " ++ err)
    Right (Left perror) -> putStrLn ("parse error: " ++ show perror)
    Left exc -> putStrLn ("i/o error: " ++ show (exc :: SomeException))
-}
{-
runchk :: String -> QParser UQuery -> TEnv ->  Either String TUQuery
runchk s parser env = case runParser parser Nothing "" s of
                        Right x -> check x env
                        Left err -> throwError . show $ err
-}

runmf = do
  db <- initErl self
  res <- runQuery (modsfuns >>= showValue') db Nothing
  --return res
  print res

runmf' db = do
  res <- runQuery (modsfuns) db Nothing
  return res                 

main :: IO ()
main = do
  a <- getArgs
  case a of
    [s]                  -> run s Nothing
--    [s] -> runmf >> return ()
    [s,'~':'/':path,pos] -> do 
           home <- getHomeDirectory
           run s (Just (home </> path,read pos))
    [s,path,pos]         -> run s (Just (path,read pos))
    _                    -> do 
           name <- getProgName 
           putStrLn ("usage: " ++ name ++ " query")


time :: (IO a) -> IO (a,NominalDiffTime)
time m = do
  t1 <- getCurrentTime
  x <- m
  t2 <- getCurrentTime
  return (x,diffUTCTime t2 t1)
