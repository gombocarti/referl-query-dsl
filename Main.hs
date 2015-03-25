import Types
import Parser
import TypeCheck
--import qualified SqDeep as Sd
import SqRefact
-- import qualified Test
import Text.Parsec (runParserT)
import Control.Monad (when)
import Control.Monad.Reader
import Control.Monad.Error
import System.Environment (getArgs, getProgName)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Control.Exception (try, SomeException)

runQuery :: Query a ->  Database -> Maybe Arg -> IO (Either String a)
runQuery q db arg = runErrorT (runReaderT (runReaderT q db) arg)

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
                db <- initErl "haskell@localhost"
                x <- runQuery (eval q [] >>= showValue) db arg
                case x of
                  Right s  -> putStrLn s
                  Left err -> putStrLn ("error: " ++ err)
          Left err -> putStrLn ("type error: " ++ err)
    Right (Left perror) -> putStrLn ("parse error: " ++ show perror)
    Left exc -> putStrLn ("i/o error: " ++ show (exc :: SomeException))

{-
runchk :: String -> QParser UQuery -> TEnv ->  Either String TUQuery
runchk s parser env = case runParser parser Nothing "" s of
                        Right x -> check x env
                        Left err -> throwError . show $ err
-}
main :: IO ()
main = do
  a <- getArgs
  case a of
    [s]                  -> run s Nothing
    [s,'~':'/':path,pos] -> do 
           home <- getHomeDirectory
           run s (Just (home </> path,read pos))
    [s,path,pos]         -> run s (Just (path,read pos))
    _                    -> do 
           name <- getProgName 
           putStrLn ("usage: " ++ name ++ " query")

