{-# LANGUAGE TypeFamilies #-}

module Sq where

import Data.List (intersect)
import qualified Data.List
import Foreign.Erlang (ErlType (..))
import Prelude hiding (elem, (==), not, (>), Int, null)
import qualified Prelude
import qualified Text.Regex.Posix ((=~))

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

type Name = String

class Named a where
    name :: a -> Name

instance Named DbModule where
    name = mname

instance Named DbFunction where
    name = fname

instance Named DbVariable where
    name = vname

instance Named DbRecord where
    name = rname

loaded :: DbModule -> Bool
loaded = undefined

functions :: DbModule -> [DbFunction]
functions = mfunctions 

records :: DbModule -> [DbRecord]
records = mrecords

class MultiLine a where
    loc :: a -> Int

instance MultiLine DbModule where 
    loc = mloc

instance MultiLine DbFunction where
    loc = floc

moduleOf :: DbFunction -> DbModule
moduleOf = fmodule

parameters :: DbFunction -> [DbVariable]
parameters = fparameters

class Referencable a where
    references :: a -> [DbExpression]

instance Referencable DbFunction where
    references = undefined

instance Referencable DbVariable where
    references = vreferences

instance Referencable DbRecord where
    references = rreferences

returns :: DbFunction -> Type
returns = undefined

exported :: DbFunction -> Bool
exported = fexported

recursivity :: DbFunction -> DbFunctionType
recursivity = frecursive

class MultiDbExpression a where
    expressions :: a -> [DbExpression]

class VariableDefs a where
    variables :: a -> [DbVariable]

instance VariableDefs DbFunction where
    variables  = concatMap evariables . fexpressions 

instance VariableDefs DbExpression where
    variables = evariables

class Typed a where
    type ValueType a
    typeOf :: a -> ValueType a

instance Typed DbExpression where
    type ValueType DbExpression = ExprType
    typeOf = etype

instance Typed DbVariable where
    type ValueType DbVariable = Type
    typeOf = vtype

body :: DbExpression -> String
body = ebody

function :: DbExpression -> DbFunction
function = efunction

bound :: DbVariable -> Either DbExpression DbFunction
bound = undefined

fields :: DbRecord -> [DbVariable]
fields = rfields

modulesOf :: DbRecord -> [DbModule]
modulesOf = rmodules

arity :: DbFunction -> Int
arity = length . parameters

calls :: DbFunction -> [DbFunction]
calls = fcalls

data DbFunctionType = NonRecursive 
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

modules :: [DbModule]
modules = rootmodules root

atFile :: DbModule
atFile = m1

atFunction :: DbFunction
atFunction = a

m1 :: DbModule
m1 = DM { mname = "m1"
        , mloc = 3
        , mfunctions = [a, b]
        , mrecords = []
        }

m2 :: DbModule
m2 = DM { mname = "m2"
        , mloc = 2
        , mfunctions = [f]
        , mrecords = [person]
        }

person :: DbRecord
person = DR { rname = "p"
            , rfields = [nameField, ageField]
            , rmodules = [m2]
            , rreferences = [newrecord]
            }

nameField :: DbVariable
nameField = DV { vname = "name"
               , vtype = String
               , vreferences = [newrecord]
               }

ageField :: DbVariable
ageField = DV { vname = "age"
              , vtype = Int
              , vreferences = [newrecord]
              }

a :: DbFunction
a = DF { fname = "a"
       , fmodule = m1
       , fexpressions = [body]
       , fexported = True
       , fparameters = [x]
       , fcalls = [b]
       , floc = 1
       , frecursive = NonRecursive
       }
    where
      body = DE { etype = FuncCall
                , ebody = "b(X + 2)."
                , efunction = a
                , evariables = [x]
                }

      x = DV { vname = "X"
             , vtype = Int
             , vreferences = [body]
             }

b :: DbFunction
b = DF { fname = "b"
       , fmodule = m1
       , fexpressions = [body]
       , fexported = False
       , fparameters = [y]
       , fcalls = []
       , floc = 2
       , frecursive = NonRecursive
       }
    where
      body = DE { etype = FuncCall
                , ebody = "F = 2," ++ 
                          "F * Y."
                , efunction = a
                , evariables = [y, z]
                }

      y = DV { vname = "Y"
             , vtype = Int
             , vreferences = [body]
             }

      z = DV { vname = "Z"
             , vtype = Int
             , vreferences = [body]
             }

nameDef = DE { etype = Plain
             , ebody = "Name = \"Géza\""
             , efunction = f
             , evariables = [nameVar]
             }

newrecord = DE { etype = Plain
               , ebody = "#p{name = Name, age = Age}"
               , efunction = f
               , evariables = [nameVar, age]
               }

age = DV { vname = "Age"
         , vtype = Int
         , vreferences = [newrecord]
         }

nameVar = DV { vname = "Name"
             , vtype = String
             , vreferences = [nameDef, newrecord]
             }

f :: DbFunction
f = DF { fname = "f"
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

data DbRoot = DRoot { rootmodules :: [DbModule] }

data DbModule = DM { mfunctions :: [DbFunction] 
                   , mloc :: Int
                   , mrecords :: [DbRecord]
                   , mname :: Name
                   }
instance Eq DbModule where
    m1 == m2 = mname m1 == mname m2

instance Show DbModule where
    show m = "module " ++ mname m

data DbFunction =  DF
    { fname :: Name
    , fmodule :: DbModule
    , fexpressions :: [DbExpression]
    --       , fvariables :: [DbVariable]
    , fparameters :: [DbVariable]
    , fexported :: Bool
    , fcalls :: [DbFunction]
    , floc :: Int
    , frecursive :: DbFunctionType
    }
                
instance Eq DbFunction where
    f1 == f2 = fmodule f1 == fmodule f2 && fname f1 == fname f2 && arity f1 == arity f2

instance Show DbFunction where
    show f = "function " ++ (name . fmodule $ f) ++ ":" ++ fname f ++ "/" ++ (show . arity $ f)

data DbExpression = DE { etype :: ExprType
                       , ebody :: String
                       , efunction :: DbFunction
                       , evariables :: [DbVariable]
                       }

instance Show DbExpression where
    show e = ebody e

data DbVariable = DV { vname :: Name
                     , vtype :: Type
                     , vreferences :: [DbExpression]
                     }

data DbRecord = DR { rname :: Name
                   , rfields :: [DbVariable]
                   , rmodules :: [DbModule]
                   , rreferences :: [DbExpression]
                   }
