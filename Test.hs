module Test where

import qualified SqDeep as D (Value(..), run)
import Sq hiding ((==))

tests :: [(String, D.Value)]
tests = [ ("{m <- modules, f <- functions m | f}", D.Seq . map D.Fun $ [a,b,f])
        , ("{m <- modules, name m == \"m\", f <- functions m, name f == \"g\" | returns f}", D.Seq []) ]

check :: String
check = run tests
    where 
      run [] = "all tests passed"
      run ((t,exp):ts) = case D.run t of
                         Right res -> if res == exp
                                      then run ts
                                      else "failed test: " ++ t
                         Left err -> "failed test: " ++ err
