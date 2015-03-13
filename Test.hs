module Test where

import SqDeep (Value(..), run, Wrap(..))
import Sq hiding ((==), Int)
import Control.Exception (catch, SomeException)

type TestCase = (String, Value)

tests :: [TestCase]
tests = [ ("{f | m <- modules, f <- functions m}", wrap [a,b,f]) -- q1
        -- q2
        , ("{type p | m <- modules, name m == \"mymod\", f <- functions m, name f == \"f\", p <- parameters f}", Seq [])
        -- q3
        , ("{returns f | m <- modules, name m == \"m1\", f <- functions m, name f == \"a\"}", Seq [wrap . freturns $ a])
        -- q4
        , ("{f | m <- modules, f <- functions m, exported f, arity f == 0}", Seq [])
        -- q5
        , ("{c | f <- functions atModule , c <- calls f}", wrap [b])
        -- q6
        , ("{m | m <- modules, not (name m =~ \"^test\")}", wrap [m1,m2])
        -- q7
        , ("{c | m <- modules, f <- functions m, c <- chainInf calls f}", Seq [Chain (Complete [SqDeep.Fun b,SqDeep.Fun a]),Chain (Complete [SqDeep.Fun b]),Chain (Complete [SqDeep.Fun f])])
        -- q8
        , ("{r | m <- modules, name m == \"io\" , f <- functions m, name f == \"format\", r <- references f}", Seq [])
        -- q9
        , ("{o | o <- origin atExpression}", wrap [bodya])
        -- q9' (j)
        , ("{o | r <- references atFunction, o <- origin r}", Seq [])
        -- q10
        , ("{r | m <- modules , f <- file m, r <- records f, name r == \"p\", f <- fields r, name f == \"name\", r <- references f}", wrap [newrecord]) -- névelfedés (r)!
        -- q11
        , ("{m | m <- modules, l <- loc m, l > 400}", Seq [])
        -- q12
        , ("{f | m <- modules, f <- functions m, l <- loc f, l < 20}", wrap [a,b,f])
        -- q13
        , ("{f | f <- functions atModule, m <- max {depth e | e <- expressions f, exprType e == Case}, m > 2}", Seq [])
        -- q14
        , ("max {depth e | f <- functions atModule, e <- expressions f, exprType e == Case}", Seq [])
        -- q15
        , ("{f | m <- modules, f <- functions m, recursivity f == NonTailRecursive}", Seq [])
        -- q16
        , ("{f | m <- modules, f <- functions m, name m == name f}", Seq [])
        -- q18
        , ("{c | m <- modules, f <- functions m, c <- chainInf calls f, count (distinct c) == 1}", Seq [Chain (Sq.Complete [SqDeep.Fun Sq.b]),Chain (Complete [SqDeep.Fun Sq.f])])
        -- q19
        , ("{c | m <- modules, f <- functions m, c <- lfp calls f}", wrap [a,b,b,f]) 
        -- q20
        , ("{c | m <- modules, f <- functions m, c <- iteration 4 calls f}", Seq [])
        -- q21
        , ("{f | m <- modules, f <- functions m, f ∈ calls f}", Seq [])
        -- q22
        , ("{f | m <- modules, f <- functions m, not (null (calls f))}", wrap [a])
        -- q23
        , ("{f | m <- modules, f <- functions m, name f ∈ {name c | c <- calls f}}", Seq [])
        -- q24
        , ("average {l | f <- functions atModule, l <- loc f}", wrap [2 :: Int])
        -- q25
        , ("{f | m <- modules, f <- functions m, any_in (calls f) {f | m <- modules, name m == \"m1\", f <- functions m}}", wrap [a])
        -- q26
        , ("{name f | m <- modules, name m == \"m1\", f <- (functions m ∪ {c | f <- functions m, c <- calls f})}", wrap . map fname $ [a,b])
        -- q27
        , ("{f | m1 <- modules, name m1 == \"m1\" , m2 <- modules, name m2 == \"m2\" , f <- (functions m1) ∪ (functions m2)}", wrap [a,b,f])
        -- q28
        , ("{path f | m <- modules, f <- file m}", wrap . map path . concatMap mfile $ [m1,m2])
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
