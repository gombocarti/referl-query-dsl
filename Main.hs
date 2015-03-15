import Types
import Parser
import TypeCheck
--import SqDeep
import SqRefact
import qualified Test
import Text.Parsec (parse,runParser)
import Control.Monad.Reader
import Control.Monad.Error (throwError)
import System.Environment (getArgs)

run :: String -> Maybe Arg -> IO ()
run s arg = case runchk s start [] of
          Right (q ::: _) -> do db <- initErl "haskell@localhost"
                                x <- runReaderT (runReaderT (eval q []) db) arg
                                s <- runReaderT (runReaderT (showValue x) db) arg
                                putStrLn s
          Left err -> putStrLn $ "error: " ++ err

runchk :: String -> QParser UQuery -> TEnv ->  Either String TUQuery
runchk s parser env = case runParser parser Nothing "" s of
                        Right x -> check x env
                        Left err -> throwError . show $ err

main :: IO ()
main = do
  a <- getArgs
  case a of
    [s] -> run s Nothing
    [s,file,pos] -> run s (Just (file,read pos))
