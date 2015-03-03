module Test where

import qualified SqDeep as D (Value(..), run, Wrap(..))
import Sq hiding ((==))
import Parser hiding (check)
import Text.Parsec (parse)

tests :: [(String, D.Value)]
tests = [ ("{m <- modules, f <- functions m | f}", D.wrap [a,b,f])
        , ("{m <- modules, name m == \"m\", f <- functions m, name f == \"g\" | returns f}", D.Seq [])
        , ("{m <- modules, name m == \"m1\", f <- functions m, name f == \"g\" | returns f}", D.Seq [])
        , ("{m <- modules, f <- functions m, exported f, arity f == 0 | f}", D.Seq [])
        , ("{f <- functions atModule , c <- calls f | c}", D.wrap [b])
        , ("{m <- modules, not (name m =~ \"^test\") | m}", D.wrap [m1,m2])
        ]

check :: IO ()
check = run tests
    where 
      run [] = putStrLn "all tests passed"
      run ((t,expected):ts) = case D.run t of
                         Right actual -> if actual == expected
                                         then run ts
                                         else putStrLn $ "failed test: " ++ t ++ "\nexpected: " ++ show expected ++ "\nactual: " ++ show actual
                         Left err -> putStrLn $ "failed test: " ++ t ++ "\nreason: " ++ err
