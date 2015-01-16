import Sq
import Prelude hiding (elem)
import Control.Monad (guard)
import Text.Regex.Posix
import Data.List (union)


-- mods.funs
a = modules >>= functions
a' = [ f | m <- modules, f <- functions m ]

-- mods[name=mymod].funs[name=f].params.type
b :: [Type]
b = do 
  m <- modules
  guard (name m == "mymod")
  f <- functions m
  guard (name f == "f")
  p <- parameters f
  return $ typeOf p

b' = [typeOf p | m <- modules, name m == "mymod",
                 f <- functions m, name f == "f",
                 p <- parameters f]

-- mods[name=m1].funs[name=g].returntypes
c = do
  m <- modules
  guard (name m == "m1")
  f <- functions m
  guard (name f == "g")
  return $ returns f

c' = [returns f | m <- modules, name m == "m1", 
                  f <- functions m, name f == "g"]
         
-- mods.funs[(exported = true) , (arity = 0)]
d = do
  m <- modules
  f <- functions m
  guard (exported f && arity f == 0)
  return f

d' = [f | m <- modules, f <- functions m, exported f, arity f == 0]

-- @file.funs.calls 
e = functions atFile >>= calls

e' = [calls f | f <- functions atFile]

-- mods[name ~ "[^test].*" ]
f = [m | m <- modules, not $ name m =~ "^test"]

-- mods.funs(.calls)+
g = undefined

-- mods[name=io].funs[name=format].refs
h = do 
  m <- modules
  guard (name m == "io")
  f <- functions m
  guard (name f == "format")
  return . references $ f

h' = [references f | m <- modules, name m == "io", 
                     f <- functions m, name f == "format"]

-- @expr.origin ???
i = undefined

-- @fun.refs.origin
--j = let f = atFunction in references f >>= origin

-- mods.records[name=p].fields[name=name].refs
k = do
  m <- modules
  r <- records m
  guard (name r == "p")
  f <- fields r
  guard (name f == "name")
  references f

k' = [references f | m <- modules,
                     r <- records m, name r == "p",
                     f <- fields r, name f == "name"]

-- mods[line_of_code > 400]
l = [m | m <- modules, loc m > 400]

-- mods.funs[line_of_code > 20]
m = [f | m <- modules, f <- functions m, loc f > 20]

-- @file.funs[max_depth_of_cases > 2]
n = undefined

-- @file.max_depth_of_cases
o = undefined

-- mods.funs[is_tail_recursive == non_tail_rec]
p = [f | m <- modules, f <- functions m, recursivity f == Recursive]

-- mods[name=A].funs[name=A]
q = [f | m <- modules, f <- functions m, name m == name f]

-- mods[name=A].funs[name=B, A=B]
r = q

-- mods.funs(.calls[name=B])+
s = undefined

-- mods.funs(.calls[name=B])+.name
-- loop!!!
t = undefined

t' = do
  m <- modules
  fs <- loop [] (functions m)
  return . name $ fs
      where
        -- ez nem tul jo rekurziv fv eseten
        loop olds [] = olds
        loop olds news = loop (olds ++ news) (concatMap calls news)

-- mods.funs{.calls}4
u = undefined

u' = do 
  m <- modules
  let fs = functions m
  loop fs calls 4 
      where
        loop fs _f 0 = fs
        loop fs  f n = loop (concatMap f fs) f (n - 1)

-- mods.funs=A.calls=A
v = [f | m <- modules, f <- functions m, f `elem` calls f]

-- mods.funs[.calls]
w = [f | m <- modules, f <- functions m, not (null (calls f))]

-- mods.funs[.calls[name=A], name=A]
x = do
  m <- modules
  f <- functions m
  let cs = calls f
      names = map name cs
  guard (name f `elem` names)
  return f

x' = [f | m <- modules, f <- functions m, name f `elem` [name c | c <- calls f]]
 
-- @file.functions.line_of_code:average
y = let lines = map loc $ functions atFile in average lines 

y' = average [loc f | f <- functions atFile]

-- mods.funs[.calls any_in mods[name=m1].funs]
z = do
  m <- modules
  f <- functions m 
  guard (calls f `any_in` m1funs)
  return f
    where
      m1funs = [f | m <- modules, name m == "m1", f <- functions m] 

z' = [f | m <- modules, f <- functions m, 
          calls f `any_in` [f | m <- modules, name m == "m1", f <- functions m] ]

-- mods[name=m1](.funs U .funs.calls).name
z1 = do
  m <- modules
  guard (name m == "m1")
  f <- functions m  
  name f : map name (calls f)

z1' = [name f | m <- modules, name m == "m1", 
                f <- (functions m `union` [c | f <- functions m , c <- calls f]) ]

-- (mods[name=m1] U mods[name=m2]).funs
z2 = do
  (m1 `union` m2) >>= functions
    where
      m1 = [m | m <- modules, name m == "m1"]
      m2 = [m | m <- modules, name m == "m2"]

z2' = concat [(functions m1) `union` (functions m2)
           | m1 <- modules, name m1 == "m1"
           , m2 <- modules, name m2 == "m2" ]
