import Types
import Parser
import TypeCheck
--import SqDeep
import SqRefact
import qualified Test
import Text.Parsec (runParser)
import Control.Monad.Reader
import Control.Monad.Error
import System.Environment (getArgs)

runQuery :: Query a ->  Database -> Maybe Arg -> IO (Either String a)
runQuery q db arg = runErrorT (runReaderT (runReaderT q db) arg)

run :: String -> Maybe Arg -> IO ()
run sq arg = case runchk sq start [] of
                  Right (q ::: _) -> do
                    db <- initErl "haskell@localhost"
                    x <- runQuery (eval q [] >>= showValue) db arg
                    case x of
                      Right s  -> putStrLn s
                      Left err -> putStrLn ("error: " ++ err)
                  Left err -> putStrLn $ "error: " ++ err

runchk :: String -> QParser UQuery -> TEnv ->  Either String TUQuery
runchk s parser env = case runParser parser Nothing "" s of
                        Right x -> check x env
                        Left err -> throwError . show $ err

main :: IO ()
main = do
  a <- getArgs
  case a of
    [s]          -> run s Nothing
    [s,file,pos] -> run s (Just (file,read pos))
    _            -> putStrLn "usage: Main query"
