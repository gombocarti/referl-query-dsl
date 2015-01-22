import Sq
import Prelude (undefined)

-- mods.funs
q1 :: [DbFunction]
q1 = [f | m <- modules, f <- functions m ]

-- mods[name=mymod].funs[name=f].params.type
q2 :: [Type]
q2 = [typeOf p | m <- modules, name m == "mymod",
                 f <- functions m, name f == "f",
                 p <- parameters f]

-- mods[name=m1].funs[name=g].returntypes
q3 :: [Type]
q3 = [returns f | m <- modules, name m == "m1", 
                  f <- functions m, name f == "g"]
         
-- mods.funs[(exported = true) , (arity = 0)]
q4 :: [DbFunction]
q4 = [f | m <- modules, f <- functions m, exported f, arity f == 0]

-- @file.funs.calls 
q5 :: [DbFunction]
q5 = [c | f <- functions atFile
        , c <- calls f]

-- mods[name ~ "[^test].*" ]
q6 :: [DbModule]
q6 = [m | m <- modules, not (name m =~ "^test")]

-- mods.funs(.calls)+
q7 :: [Chain DbFunction]
q7 = [c | m <- modules, f <- functions m, c <- chainInf calls f]

-- mods[name=io].funs[name=format].refs
q8 :: [DbExpression]
q8 = [r | m <- modules, name m == "io"
        , f <- functions m, name f == "format"
        , r <- references f]

-- @expr.origin
q9 = [o | o <- origin atExpression]

-- @fun.refs.origin
j = [o | r <- references atFunction, o <- origin r]

-- mods.records[name=p].fields[name=name].refs
q10 :: [DbExpression]
q10 = [r | m <- modules
         , r <- records m, name r == "p"
         , f <- fields r, name f == "name"
         , r <- references f]

-- mods[line_of_code > 400]
q11 :: [DbModule]
q11 = [m | m <- modules, loc m > 400]

-- mods.funs[line_of_code > 20]
q12 :: [DbFunction]
q12 = [f | m <- modules, f <- functions m, loc f > 20]

-- @file.funs[max_depth_of_cases > 2]
q13 :: [DbFunction]
q13 = undefined

-- @file.max_depth_of_cases
q14 :: Int
q14 = undefined

-- mods.funs[is_tail_recursive == non_tail_rec]
q15 :: [DbFunction]
q15 = [f | m <- modules, f <- functions m, recursivity f == NonTailRecursive]

-- mods[name=A].funs[name=A]
q16 :: [DbFunction]
q16 = [f | m <- modules, f <- functions m, name m == name f]

-- mods[name=A].funs[name=B, A=B]
q17 :: [DbFunction]
q17 = q16

-- mods.funs(.calls[name=B])+
-- so-so
q18 :: [Chain DbFunction]
q18 = [c | m <- modules, f <- functions m, c <- chainInf (\g -> [c | c <- calls g, name c == name g ]) f]

-- mods.funs(.calls[name=B])+.name
q19 :: [Name]
q19 = [name c | m <- modules, f <- functions m, c <- closureInf calls f]

-- mods.funs{.calls}4
q20 :: [[DbFunction]]
q20 = undefined

-- mods.funs=A.calls=A
q21 :: [DbFunction]
q21 = [f | m <- modules, f <- functions m, f `elem` calls f]

-- mods.funs[.calls]
q22 :: [DbFunction]
q22 = [f | m <- modules, f <- functions m, not (null (calls f))]

-- mods.funs[.calls[name=A], name=A]
q23 :: [DbFunction]
q23 = [f | m <- modules, f <- functions m, name f `elem` [name c | c <- calls f]]
 
-- @file.functions.line_of_code:average
q24 :: Int
q24 = average [loc f | f <- functions atFile]

-- mods.funs[.calls any_in mods[name=m1].funs]
q25 :: [DbFunction]
q25 = [f | m <- modules, f <- functions m, 
          calls f `any_in` [f | m <- modules, name m == "m1", f <- functions m] ]

-- mods[name=m1](.funs U .funs.calls).name
q26 :: [Name]
q26 = [name f | m <- modules, name m == "m1", 
                f <- (functions m `u` [c | f <- functions m , c <- calls f]) ]

-- (mods[name=m1] U mods[name=m2]).funs
q27 :: [DbFunction]
q27 = [f | m1 <- modules, name m1 == "m1"
         , m2 <- modules, name m2 == "m2"
         , f <- (functions m1) `u` (functions m2)]
