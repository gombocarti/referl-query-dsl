{-# LANGUAGE TypeFamilies #-}

module Sq where

import Data.List (intersect)
import qualified Data.List
import Foreign.Erlang (ErlType (..))
import Prelude hiding (elem, (==), not, (>), Int, null)
import qualified Prelude
import qualified Text.Regex.Posix ((=~))

-- import qualified Data.Set as Set (Set)

{-
data Node = Module DbModule
          | Function DbFunction
          | Expression DbExpression
          | Variable DbVariable
          | Record DbRecord

loaded :: Node -> Bool
loaded (Module m) = True

name :: Node -> String
name (Module m) = mname m

functions :: Node -> [Node]
functions (Module m) = map FunDbModulection $ mfunctions m

  -}

type Int = Prelude.Int
    
(==) :: Eq a => a -> a -> Bool
(==) = (Prelude.==)

(>) :: Ord a => a -> a -> Bool
(>) = (Prelude.>)

not :: Bool -> Bool
not = Prelude.not

null :: [a] -> Bool
null = Prelude.null

-- union :: [[a]] -> [a]
-- union = concat

u :: Eq a => [a] -> [a] -> [a]
u = Data.List.union

(=~) :: Name -> String -> Bool
(=~) = (Text.Regex.Posix.=~)
        
newtype Module = M { getM :: DbModule }
    deriving (Show, Eq)

newtype Function = F { getF ::  DbFunction }
    deriving (Show, Eq)

newtype Expression = E { getE :: DbExpression }
    deriving Show

newtype Variable = V { getV :: DbVariable }

newtype Record = R { getR :: DbRecord }

type Name = String

class Named a where
    name :: a -> Name

instance Named Module where
    name (M m) = mname m

instance Named Function where
    name (F f) = fname f

instance Named Variable where
    name (V v) = vname v

instance Named Record where
    name = rname . getR

loaded :: Module -> Bool
loaded = undefined

functions :: Module -> [Function]
functions (M m) = mfunctions m

records :: Module -> [Record]
records (M m) = mrecords m

class MultiLine a where
    loc :: a -> Int

instance MultiLine Module where 
    loc (M m) = mloc m

instance MultiLine Function where
    loc (F f) = floc f

moduleOf :: Function -> Module
moduleOf (F f) = fmodule f

parameters :: Function -> [Variable]
parameters (F f) = fparameters f

class Referencable a where
    references :: a -> [Expression]

instance Referencable Function where
    references (F f) = undefined

instance Referencable Variable where
    references (V v) = vreferences v

instance Referencable Record where
    references (R r) = rreferences r

returns :: Function -> Type
returns = undefined

exported :: Function -> Bool
exported (F f) = fexported f

recursivity :: Function -> FunctionType
recursivity = frecursive . getF

class MultiExpression a where
    expressions :: a -> [Expression]

class VariableDefs a where
    variables :: a -> [Variable]

instance VariableDefs Function where
    variables (F f) = concatMap (evariables . getE) $ fexpressions f

instance VariableDefs Expression where
    variables (E e) = evariables e

class Typed a where
    type ValueType a
    typeOf :: a -> ValueType a

instance Typed Expression where
    type ValueType Expression = ExprType
    typeOf e = undefined

instance Typed Variable where
    type ValueType Variable = Type
    typeOf v = undefined

body :: Expression -> String
body = ebody . getE

function :: Expression -> Function
function = efunction . getE

bound :: Variable -> Either Expression Function
bound = undefined

fields :: Record -> [Variable]
fields = rfields . getR

modulesOf :: Record -> [Module]
modulesOf = rmodules . getR

arity :: Function -> Int
arity = length . parameters

calls :: Function -> [Function]
calls = fcalls . getF

data FunctionType = NonRecursive 
                  | Recursive
                  | TailRecursive
                    deriving (Show, Eq)
                   

data Type = Atom
          | String
          | Int
          | Bool
          | ListOf Type
          | Either Type Type
            deriving Show

data ExprType = Plain
              | FuncCall

subset :: Eq a => [a] -> [a] -> Bool
subset xs ys = all (`elem` ys) xs

average :: [Int] -> Int
average xs = round $ (fromIntegral $ sum xs) / (fromIntegral $ length xs)

any_in :: Eq a => [a] -> [a] -> Bool
any_in xs ys = not (null (xs `intersect` ys))

all_in :: Eq a => [a] -> [a] -> Bool
all_in = subset

elem :: Eq a => a -> [a] -> Bool
elem = Prelude.elem

modules :: [Module]
modules = rootmodules root

atFile :: Module
atFile = m1

atFunction :: Function
atFunction = a

m1 :: Module
m1 = M $ DM { mname = "m1"
            , mloc = 3
            , mfunctions = [a, b]
            , mrecords = []
            }

m2 :: Module
m2 = M $ DM { mname = "m2"
            , mloc = 2
            , mfunctions = [f]
            , mrecords = [person]
            }

person :: Record
person = R $ DR { rname = "p"
                , rfields = [nameField, ageField]
                , rmodules = [m2]
                , rreferences = [newrecord]
                }

nameField :: Variable
nameField = V $ DV { vname = "name"
                   , vtype = String
                   , vreferences = [newrecord]
                   }

ageField :: Variable
ageField = V $ DV { vname = "age"
                  , vtype = Int
                  , vreferences = [newrecord]
                  }

a :: Function
a = F $ DF { fname = "a"
           , fmodule = m1
           , fexpressions = [body]
           , fexported = True
           , fparameters = [x]
           , fcalls = [b]
           , floc = 1
           , frecursive = NonRecursive
           }
    where
      body = E $ DE { etype = FuncCall
                    , ebody = "b(X + 2)."
                    , efunction = a
                    , evariables = [x]
                    }

      x = V $ DV { vname = "X"
                 , vtype = Int
                 , vreferences = [body]
                 }

b :: Function
b = F $ DF { fname = "b"
           , fmodule = m1
           , fexpressions = [body]
           , fexported = False
           , fparameters = [y]
           , fcalls = []
           , floc = 2
           , frecursive = NonRecursive
           }
    where
      body = E $ DE { etype = FuncCall
                    , ebody = "F = 2," ++ 
                              "F * Y."
                    , efunction = a
                    , evariables = [y, z]
                    }

      y = V $ DV { vname = "Y"
                 , vtype = Int
                 , vreferences = [body]
                 }

      z = V $ DV { vname = "Z"
                 , vtype = Int
                 , vreferences = [body]
                 }

nameDef = E $ DE { etype = Plain
                 , ebody = "Name = \"Géza\""
                 , efunction = f
                 , evariables = [nameVar]
                 }

newrecord = E $ DE { etype = Plain
                   , ebody = "#p{name = Name, age = Age}"
                   , efunction = f
                   , evariables = [nameVar, age]
                   }

age = V $ DV { vname = "Age"
             , vtype = Int
             , vreferences = [newrecord]
             }

nameVar = V $ DV { vname = "Name"
                 , vtype = String
                 , vreferences = [nameDef, newrecord]
                 }

f :: Function
f = F $ DF { fname = "f"
           , fmodule = m2
           , fexpressions = [nameDef, newrecord]
           , fparameters = [age]
           , fexported = False
           , fcalls = []
           , floc = 2
           , frecursive = NonRecursive
           }

root :: DbRoot
root = DRoot [m1, m2]

data DbRoot = DRoot { rootmodules :: [Module] }

data DbModule = DM { mfunctions :: [Function] 
                   , mloc :: Int
                   , mrecords :: [Record]
                   , mname :: Name
                   }
instance Eq DbModule where
    m1 == m2 = mname m1 == mname m2

instance Show DbModule where
    show m = "module " ++ mname m

data DbFunction =  DF
    { fname :: Name
    , fmodule :: Module
    , fexpressions :: [Expression]
    --       , fvariables :: [Variable]
    , fparameters :: [Variable]
    , fexported :: Bool
    , fcalls :: [Function]
    , floc :: Int
    , frecursive :: FunctionType
    }
                
instance Eq DbFunction where
    f1 == f2 = fmodule f1 == fmodule f2 && fname f1 == fname f2 && arity (F f1) == arity (F f2)

instance Show DbFunction where
    show f = "function " ++ fname f

data DbExpression = DE { etype :: ExprType
                       , ebody :: String
                       , efunction :: Function
                       , evariables :: [Variable]
                       }

instance Show DbExpression where
    show e = ebody e

data DbVariable = DV { vname :: Name
                     , vtype :: Type
                     , vreferences :: [Expression]
                     }

data DbRecord = DR { rname :: Name
                   , rfields :: [Variable]
                   , rmodules :: [Module]
                   , rreferences :: [Expression]
                   }
