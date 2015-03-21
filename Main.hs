import Types
import Parser
import TypeCheck
import qualified SqDeep as Sd
import SqRefact
-- import qualified Test
import Text.Parsec (runParserT)
import Control.Monad.Reader
import Control.Monad.Error
import System.Environment (getArgs, getProgName)
import Control.Exception (try, SomeException)

runQuery :: Query a ->  Database -> Maybe Arg -> IO (Either String a)
runQuery q db arg = runErrorT (runReaderT (runReaderT q db) arg)

run :: String -> Maybe Arg -> IO ()
run sq arg = do
  parseResult <- try (runParserT query Nothing "" sq)
  case parseResult of
    Right (Right tree) ->
        case check tree [] of
          Right (q ::: _) -> do
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
    [s]          -> run s Nothing
    [s,file,pos] -> run s (Just (file,read pos))
    _            -> do 
           name <- getProgName 
           putStrLn ("usage: " ++ name ++ " query")

