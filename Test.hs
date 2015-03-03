module Test where

import qualified SqDeep as D (Value(..), run, Wrap(..))
import Sq hiding ((==), Int)
import Parser hiding (check)
import Text.Parsec (parse)

tests :: [(String, D.Value)]
tests = [ ("{m <- modules, f <- functions m | f}", D.wrap [a,b,f]) -- q1
        -- q2
        , ("{m <- modules, name m == \"mymod\", f <- functions m, name f == \"f\", p <- parameters f | typeOf p}", D.Seq [])
        -- q3
        , ("{m <- modules, name m == \"m1\", f <- functions m, name f == \"a\" | returns f}", D.wrap [a])
        -- q4
        , ("{m <- modules, f <- functions m, exported f, arity f == 0 | f}", D.Seq [])
        -- q5
        , ("{f <- functions atModule , c <- calls f | c}", D.wrap [b])
        -- q6
        , ("{m <- modules, not (name m =~ \"^test\") | m}", D.wrap [m1,m2])
        -- q7
        , ("{m <- modules, f <- functions m, c <- chainInf calls f | c}", D.Seq [])
        -- q8
        , ("{m <- modules, name m == \"io\" , f <- functions m, name f == \"format\", r <- references f | r}", D.Seq [])
        -- q9
        , ("{o <- origin atExpression | o}", D.wrap [bodya])
        -- q9' (j)
        , ("{r <- references atFunction, o <- origin r | o}", D.Seq [])
        -- q10
        , ("{m <- modules , file <- mfile m, r <- records file, name r == \"p\", f <- fields r, name f == \"name\", r <- references f | r}", D.wrap [newrecord]) -- névelfedés (r)!
        -- q11
        , ("{m <- modules, l <- loc m, l > 400 | m}", D.Seq [])
        -- q12
        , ("{m <- modules, f <- functions m, l <- loc f, l < 20 | f)", D.wrap [a,b,f])
        -- q13
        , ("{f <- functions atModule, m <- max [depth e | e <- expressions f, typeOf e == Case], m > 2 | f}", D.Seq [])
        -- q14
        , ("max {f <- functions atModule, e <- expressions f, typeOf e == Case | depth e}", D.Seq [])
        -- q15
        , ("{m <- modules, f <- functions m, recursivity f == NonTailRecursive | f}", D.Seq [])
        -- q16
        , ("{m <- modules, f <- functions m, name m == name f | f}", D.Seq [])
        -- q18
        , ("{m <- modules, f <- functions m, c <- chainInf (\\g -> [c | c <- calls g, name c == name g ]) f | c}", D.Seq []) -- ennek nem üres lista az eredménye
        -- q19
        , ("{m <- modules, f <- functions m, c <- lfp calls f | c}", D.Seq []) -- ennek sem
        -- q20
        , ("{m <- modules, f <- functions m, c <- iteration 4 calls f | c}", D.Seq [])
        -- q21
        , ("{m <- modules, f <- functions m, f `elem` calls f | f}", D.Seq [])
        -- q22
        , ("{m <- modules, f <- functions m, not (null (calls f)) | f}", D.wrap [a])
        -- q23
        , ("{m <- modules, f <- functions m, name f `elem` {name c | c <- calls f} | f}", D.Seq [])
        -- q24
        , ("average {f <- functions atModule, l <- loc f| l}", D.wrap (2 :: Int))
        -- q25
        , ("{m <- modules, f <- functions m, calls f `any_in` {f | m <- modules, name m == \"m1\", f <- functions m} | f}", D.wrap [a])
        -- q26
        , ("{m <- modules, name m == \"m1\", f <- (functions m `u` {c | f <- functions m , c <- calls f} | name f}", D.wrap . map fname $ [a,b])
        -- q27
        , ("{m1 <- modules, name m1 == \"m1\" , m2 <- modules, name m2 == \"m2\" , f <- (functions m1) `u` (functions m2)}", D.wrap [a,b,f])
        -- q28
        , ("{m <- modules, f <- mfile m | path f}", D.wrap . map path . concatMap mfile $ [m1,m2])
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
