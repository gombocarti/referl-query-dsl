module Test where

import SqDeep (Value(..), run, Wrap(..))
import Sq hiding ((==), Int)
import Control.Exception (catch, SomeException)

type TestCase = (String, Value)

tests :: [TestCase]
tests = [ ("{m <- modules, f <- functions m | f}", wrap [a,b,f]) -- q1
        -- q2
        , ("{m <- modules, name m == \"mymod\", f <- functions m, name f == \"f\", p <- parameters f | type p}", Seq [])
        -- q3
        , ("{m <- modules, name m == \"m1\", f <- functions m, name f == \"a\" | returns f}", Seq [wrap . freturns $ a])
        -- q4
        , ("{m <- modules, f <- functions m, exported f, arity f == 0 | f}", Seq [])
        -- q5
        , ("{f <- functions atModule , c <- calls f | c}", wrap [b])
        -- q6
        , ("{m <- modules, not (name m =~ \"^test\") | m}", wrap [m1,m2])
        -- q7
        , ("{m <- modules, f <- functions m, c <- chainInf calls f | c}", Seq [Chain (Complete [SqDeep.Fun b,SqDeep.Fun a]),Chain (Complete [SqDeep.Fun b]),Chain (Complete [SqDeep.Fun f])])
        -- q8
        , ("{m <- modules, name m == \"io\" , f <- functions m, name f == \"format\", r <- references f | r}", Seq [])
        -- q9
        , ("{o <- origin atExpression | o}", wrap [bodya])
        -- q9' (j)
        , ("{r <- references atFunction, o <- origin r | o}", Seq [])
        -- q10
        , ("{m <- modules , f <- file m, r <- records f, name r == \"p\", f <- fields r, name f == \"name\", r <- references f | r}", wrap [newrecord]) -- névelfedés (r)!
        -- q11
        , ("{m <- modules, l <- loc m, l > 400 | m}", Seq [])
        -- q12
        , ("{m <- modules, f <- functions m, l <- loc f, l < 20 | f}", wrap [a,b,f])
        -- q13
        , ("{f <- functions atModule, m <- max {e <- expressions f, exprType e == Case | dept e}, m > 2 | f}", Seq [])
        -- q14
        , ("max {f <- functions atModule, e <- expressions f, type e == Case | depth e}", Seq [])
        -- q15
        , ("{m <- modules, f <- functions m, recursivity f == NonTailRecursive | f}", Seq [])
        -- q16
        , ("{m <- modules, f <- functions m, name m == name f | f}", Seq [])
        -- q18
        , ("{m <- modules, f <- functions m, c <- chainInf (\\g -> [c | c <- calls g, name c == name g ]) f | c}", Seq []) -- ennek nem üres lista az eredménye
        -- q19
        , ("{m <- modules, f <- functions m, c <- lfp calls f | c}", Seq []) -- ennek sem
        -- q20
        , ("{m <- modules, f <- functions m, c <- iteration 4 calls f | c}", Seq [])
        -- q21
        , ("{m <- modules, f <- functions m, f ∈ calls f | f}", Seq [])
        -- q22
        , ("{m <- modules, f <- functions m, not (null (calls f)) | f}", wrap [a])
        -- q23
        , ("{m <- modules, f <- functions m, name f ∈ {c <- calls f | name c} | f}", Seq [])
        -- q24
        , ("average {f <- functions atModule, l <- loc f | l}", wrap (2 :: Int))
        -- q25
        , ("{m <- modules, f <- functions m, any_in (calls f) {m <- modules, name m == \"m1\", f <- functions m | f} | f}", wrap [a])
        -- q26
        , ("{m <- modules, name m == \"m1\", f <- (functions m ∪ {f <- functions m , c <- calls f | c}) | name f}", wrap . map fname $ [a,b])
        -- q27
        , ("{m1 <- modules, name m1 == \"m1\" , m2 <- modules, name m2 == \"m2\" , f <- (functions m1) ∪ (functions m2) | f}", wrap [a,b,f])
        -- q28
        , ("{m <- modules, f <- file m | path f}", wrap . map path . concatMap mfile $ [m1,m2])
        ]

check :: [TestCase] -> IO ()
check ts = runAll ts 0 0
    where 
      runAll :: [TestCase] -> Int -> Int -> IO ()
      runAll [] p f = putStrLn $ "passed " ++ show p ++ " failed " ++ show f
      runAll ((t,expected):ts) p f =
                 (case run t of
                    Right actual -> if actual == expected
                                    then runAll ts (p + 1) f
                                    else do
                                      putStrLn $ "failed test: " ++ t ++ "\nexpected: " ++ show expected ++ "\nactual: " ++ show actual
                                      runAll ts p (f + 1)
                    Left err -> do 
                      putStrLn $ "failed test: " ++ t ++ "\nreason: " ++ err
                      runAll ts p (f + 1))
                 `catch` (\e -> putStrLn $ "failed test: " ++ t ++ "\nreason: " ++ show (e :: SomeException)) 
