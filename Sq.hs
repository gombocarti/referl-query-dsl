{-# LANGUAGE TypeFamilies #-}

module Sq where

import Data.List (intersect)
import qualified Data.List
import Prelude hiding (elem, (==), not, (>), Int, null)
import qualified Prelude
import qualified Text.Regex.Posix ((=~))
import Data.Functor
import Data.Function (on)
import qualified System.FilePath(FilePath, takeFileName, takeBaseName, takeDirectory)

-- re-exported types and functions

type Int = Prelude.Int

type FilePath = System.FilePath.FilePath

(==) :: Eq a => a -> a -> Bool
(==) = (Prelude.==)

(>) :: Ord a => a -> a -> Bool
(>) = (Prelude.>)

not :: Bool -> Bool
not = Prelude.not

null :: [a] -> Bool
null = Prelude.null

(=~) :: Name -> String -> Bool
(=~) = (Text.Regex.Posix.=~)

elem :: Eq a => a -> [a] -> Bool
elem = Prelude.elem

-- union :: [[a]] -> [a]
-- union = concat

u :: Eq a => [a] -> [a] -> [a]
u = Data.List.union

-- closures, chains

closureN :: Int -> (a -> [a]) -> a -> [a]
closureN n f x = concat . take n . iterate (concatMap f) $ [x]

lfp :: Eq a => (a -> [a]) -> a -> [a]
lfp f xs = loop [] [xs]
    where loop old curr = let old' = old `u` curr
                              new = concatMap f curr in
                           if new `subset` old'
                           then old'
                           else loop old' new

iteration :: Eq a => Int -> (a -> [a]) -> a -> [a]
iteration n f x = iterate (concatMap f) [x] !! n

data Chain a = Incomplete [a]
             | Complete [a]
             | Recursive [a]
               deriving (Show,Eq)

chainInf :: Eq a => (a -> [a]) -> a -> [Chain a]
chainInf f x = loop [] [Incomplete [x]]
    where loop finished [] = finished
          loop finished unfinished = let new = concatMap cont unfinished
                                         (finished', unfinished') = split new
                                     in loop (finished' ++ finished) unfinished'

          cont (Incomplete chain@(z:_)) = case f z of
                                            [] -> [Complete chain]
                                            ys -> [classify y chain | y <- ys]

          classify y chain | y `elem` chain = Recursive chain
                           | otherwise      = Incomplete (y:chain)

          split chains = Data.List.partition isComplete chains 

          isComplete (Complete _) = True
          isComplete _            = False

{-
-- TODO
chainN :: Int -> (a -> [a]) -> [a] -> [[a]]
chainN n f xs = 
-}

{-
-- pointless:

chainIteration :: Int -> (a -> [a]) -> a -> [Chain a]
chainIteration n f x = Complete <$> loop n [[x]]
    where loop 0 chains = chains
          loop n chains = loop (n - 1) (concatMap cont chains)

          cont chain@(x:_)   = case f x of 
                                  [] -> [chain]
                                  ys -> [y:chain | y <- ys]
-}

-- selectors and properties

type Name = String

class Named a where
    name :: a -> Name

instance Named DbFile where
    name = System.FilePath.takeBaseName . fpath

instance Named DbModule where
    name = mname

instance Named DbFunction where
    name = fname

instance Named DbVariable where
    name = vname

instance Named DbRecord where
    name = rname

instance Named DbRecordField where
    name = fieldName


functions :: DbModule -> [DbFunction]
functions = mfunctions 

records :: DbFile -> [DbRecord]
records = frecords

class MultiLine a where
    loc :: a -> [Int]

instance MultiLine DbFile where
    loc f = [fileLoc f]

instance MultiLine DbModule where 
    loc = map fileLoc . mfile

instance MultiLine DbFunction where
    loc = floc

moduleOf :: DbFunction -> DbModule
moduleOf = fmodule

parameters :: DbFunction -> [DbVariable]
parameters = fparameters

class Referencable a where
    references :: a -> [DbExpression]

instance Referencable DbFunction where
    references = freferences

instance Referencable DbVariable where
    references = vreferences

instance Referencable DbRecord where
    references = rreferences

instance Referencable DbRecordField where
    references = fieldReferences

returns :: DbFunction -> [DbType]
returns = freturns

exported :: DbFunction -> Bool
exported = fexported

recursivity :: DbFunction -> DbFunctionType
recursivity = frecursive

class MultiExpression a where
    expressions :: a -> [DbExpression]

instance MultiExpression DbExpression where
    expressions = eexpressions

instance MultiExpression DbFunction where
    expressions = fexpressions

depth :: DbExpression -> Int
depth = undefined

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

class FunctionScope a where
    function :: a -> DbFunction

instance FunctionScope DbExpression where
    function = efunction

instance FunctionScope DbVariable where
    function = efunction . head . vbindings

body :: DbExpression -> String
body = ebody

bound :: DbVariable -> Either DbExpression DbFunction
bound = undefined

fields :: DbRecord -> [DbRecordField]
fields = rfields

modulesOf :: DbRecord -> [DbModule]
modulesOf = rmodules

arity :: DbFunction -> Int
arity = length . parameters

calls :: DbFunction -> [DbFunction]
calls = fcalls

data DbFunctionType = NonRecursive 
                    | NonTailRecursive
                    | TailRecursive
                      deriving (Show,Eq,Read)
                   
data ExprType
    = Application
    | Implicit_fun
    | Fun_expr
    | Tuple
    | List
    | Integer
    | Char
    | Float
    | String
    | Variable
    | Cons
    | Record_expr
    | Record_update
    | Record_access
    | Match_expr
    | Infix_expr
    | Case
    | If_expr
    | Send_expr
    | Receive_expr  
    | Try_expr      -- ^ try ... end
    | Catch_expr
    | Block_expr    -- ^ begin ... end
      deriving (Show,Eq,Read)

filename :: DbFile -> System.FilePath.FilePath
filename = System.FilePath.takeFileName . fpath

dir :: DbFile -> System.FilePath.FilePath
dir = System.FilePath.takeDirectory . fpath

path :: DbFile -> System.FilePath.FilePath
path = fpath

data FileType 
    = Module
    | Header
      deriving (Eq, Show)

is_module :: DbFile -> Bool
is_module = fismodule

is_header :: DbFile -> Bool
is_header = not . fismodule

-- grouping 

newtype Criteria a = C (a -> a -> Bool)

cmodule :: Criteria DbFunction
cmodule = C ((==) `on` fmodule)

groupBy :: [a] -> Criteria a -> [[a]]
groupBy xs (C c) = Data.List.groupBy c xs

type Grouped a b = [(b, [a])]

groupBy' :: Eq b => [a] -> (a -> b) -> Grouped a b
groupBy' xs f = zip grouping grouped
    where 
      grouped = Data.List.groupBy ((==) `on` f) xs
      grouping = map (f . head) grouped

-- set functions

subset :: Eq a => [a] -> [a] -> Bool
subset xs ys = all (`elem` ys) xs

any_in :: Eq a => [a] -> [a] -> Bool
any_in xs ys = not (null (xs `intersect` ys))

all_in :: Eq a => [a] -> [a] -> Bool
all_in = subset

-- aggregate functions

average :: [Int] -> [Int]
average [] = []
average xs = [round $ (fromIntegral $ sum xs) / (fromIntegral $ length xs)]

max :: Ord a => [a] -> [a]
max [] = []
max xs = [maximum xs]

min :: Ord a => [a] -> [a]
min [] = []
min xs = [minimum xs]

-- initial selectors

files :: [DbFile]
files = rootfiles root

modules :: [DbModule]
modules = rootmodules root

atFile :: DbFile
atFile = head . mfile $ atModule

atModule :: DbModule
atModule = m1

atFunction :: DbFunction
atFunction = a

atExpression :: DbExpression
atExpression = bodya

-- database prototype

m1 :: DbModule
m1 = DM { mname = "m1"
        , mfunctions = [a, b]
        , mfile = [m1File]
        , mexports = []
        , mimports = []
        }

m1File :: DbFile
m1File = DFile { fismodule = True
               , fpath = "/home/r2r/m1.erl"
               , fincludes = []
               , fincluded_by = []
               , fmacros = []
               , fileLoc = 3
               , frecords = []
               , fileModule = [m1]
               , fspecs = []
               , ftypes = []
               , ftypeReferences = []
               }

m2 :: DbModule
m2 = DM { mname = "m2"
        , mfile = [m2File]
        , mfunctions = [f]
        , mexports = []
        , mimports = []
        }

m2File :: DbFile
m2File = DFile { fileLoc = 2
               , frecords = [person]
               , fpath = "/home/r2r/m2.erl"
               , fismodule = True
               , fincludes = []
               , fincluded_by = []
               , fmacros = []
               , fileModule = [m2]
               , fspecs = []
               , ftypes = []
               , ftypeReferences = []
               }

person :: DbRecord
person = DR { rname = "p"
            , rfields = [nameField, ageField]
            , rmodules = [m2]
            , rreferences = [newrecord]
            }

nameField :: DbRecordField
nameField = DRF { fieldName = "name"
                , fieldRecord = person
                , fieldReferences = [newrecord]
                , fieldType = stringType
                }

stringType :: DbType
stringType = emptyType { tname = "string" }

intType :: DbType
intType = emptyType { tname = "int"
                    , tbuiltin = True
                    }

emptyType :: DbType
emptyType = 
    DT 
    { tname = ""
    , tarity = 0
    , texported = False
    , topaque = False
    , tbuiltin = False
    , tfile = []
    , targuments = []
    , tsubtypes = []
    , tspecReferences = []
    , tparameterReferences = []
    }
              

ageField :: DbRecordField
ageField = DRF { fieldName = "age"
               , fieldRecord = person
               , fieldReferences = [newrecord]
               , fieldType = intType
               }

a :: DbFunction
a = DF { fname = "a"
       , fmodule = m1
       , fexpressions = [bodya]
       , fexported = True
       , fparameters = [x]
       , freturns = []
       , fcalls = [b]
       , floc = [1]
       , frecursive = NonRecursive
       , freferences = []
       , fdynamicReferences = []
       , fbif = False
       , fpure = True
       , fspec = []
       , floaded = True
       }

x = DV { vname = "X"
       , vreferences = [bodya]
       , vbindings = []
       }

bodya = DE { etype = Application
           , ebody = "b(X + 2)."
           , efunction = a
           , evariables = [x]
           , origin = [bodya]
           , reach = []
           , eexpressions = []
           }

b :: DbFunction
b = DF { fname = "b"
       , fmodule = m1
       , fexpressions = [body]
       , fexported = False
       , fparameters = [y]
       , freturns = []
       , fcalls = []
       , floc = [2]
       , frecursive = NonRecursive
       , freferences = [bodya]
       , fdynamicReferences = []
       , fbif = False
       , fpure = True
       , fspec = []
       , floaded = True
       }
    where
      body = DE { etype = Application
                , ebody = "Z = 2," ++ 
                          "Z * Y."
                , efunction = a
                , evariables = [y, z]
                , origin = [body]
                , reach = []
                , eexpressions = []
                }

      y = DV { vname = "Y"
             , vreferences = [body]
             , vbindings = [body]
             }

      z = DV { vname = "Z"
             , vreferences = [body]
             , vbindings = [body]
             }

nameDef = DE { etype = Match_expr
             , ebody = "Name = \"Géza\""
             , efunction = f
             , evariables = [nameVar]
             , origin = [nameDef]
             , reach = []
             , eexpressions = []
             }

newrecord = DE { etype = Record_expr
               , ebody = "#p{name = Name, age = Age}"
               , efunction = f
               , evariables = [nameVar, age]
               , origin = [newrecord]
               , reach = []
               , eexpressions = []
               }

age = DV { vname = "Age"
         , vreferences = [newrecord]
         , vbindings = []
         }

nameVar = DV { vname = "Name"
             , vreferences = [nameDef, newrecord]
             , vbindings = [nameDef]
             }

f :: DbFunction
f = DF { fname = "f"
       , fmodule = m2
       , fexpressions = [nameDef, newrecord]
       , fparameters = [age]
       , freturns = []
       , fexported = False
       , fcalls = []
       , floc = [2]
       , frecursive = NonRecursive
       , freferences = []
       , fdynamicReferences = []
       , fbif = False
       , fpure = True
       , floaded = True
       , fspec = []
       }

-- concepts

root :: DbRoot
root = DRoot { rootmodules = [m1, m2]
             , rootfiles = [m1File, m2File]
             }

data DbRoot = 
    DRoot { rootmodules :: [DbModule]
          , rootfiles :: [DbFile] 
          }

data DbModule =
    DM { mfunctions :: [DbFunction] 
       , mname :: Name
       , mfile :: [DbFile]
       , mexports :: [DbFunction]
       , mimports :: [DbFunction]
       }

data DbFile = 
    DFile { fismodule :: Bool
          , fpath :: System.FilePath.FilePath
          , fincludes :: [DbFile]
          , fincluded_by :: [DbFile]
          , fmacros :: [DbMacro] 
          , fileLoc :: Int
          , frecords :: [DbRecord]
          , fileModule :: [DbModule]
          , fspecs :: [DbSpec]
          , ftypes :: [DbType]
          , ftypeReferences :: [DbType]
          }
    deriving (Show,Eq)


instance Eq DbModule where
    m1 == m2 = mname m1 == mname m2

instance Show DbModule where
    show = mname

data DbMacro =
    DMa { maName :: Name
        , maArity :: Int
        , maConstant :: Bool
        , maBody :: String
        , maRefernces :: [DbExpression]
        , maFile :: [DbModule]
        }
    deriving (Show, Eq)


data DbFunction =
    DF { fname :: Name
       , fmodule :: DbModule
       , fexpressions :: [DbExpression]
       , fparameters :: [DbVariable]
       , freturns :: [DbType]
       , freferences :: [DbExpression]
       , fdynamicReferences :: [DbExpression]
       , fexported :: Bool
       , fcalls :: [DbFunction]
       , floc :: [Int]
       , frecursive :: DbFunctionType
       , fbif :: Bool
       , fpure :: Bool
       , fspec :: [DbSpec]
       , floaded :: Bool
       }

dirty :: DbFunction -> Bool
dirty = not . fpure

file :: DbExpression -> [DbFile]
file = mfile . fmodule . efunction

instance Eq DbFunction where
    f1 == f2 = fmodule f1 == fmodule f2 && fname f1 == fname f2 && arity f1 == arity f2

instance Show DbFunction where
    show f = (name . fmodule $ f) ++ ":" ++ fname f ++ "/" ++ (show . arity $ f)

data DbExpression =
    DE { etype :: ExprType
       , ebody :: String
       , efunction :: DbFunction
       , evariables :: [DbVariable]
       , origin :: [DbExpression]
       , reach :: [DbExpression]
       , eexpressions :: [DbExpression]
       }

instance Eq DbExpression where
    a == b = ebody a == ebody b && efunction a == efunction b

data DbFunctionParam =
    DFP { fpexpr :: DbExpression
        , fptype :: DbType
        , fpindex :: Int
        }
    deriving (Eq,Show)

instance Show DbExpression where
    show = ebody

data DbVariable =
    DV { vname :: Name
       , vreferences :: [DbExpression]
       , vbindings :: [DbExpression]
       }
    deriving (Show, Eq)

data DbRecord =
    DR { rname :: Name
       , rfields :: [DbRecordField]
       , rmodules :: [DbModule]
       , rreferences :: [DbExpression]
       }
    deriving (Show, Eq)

data DbRecordField =
    DRF { fieldName :: Name
        , fieldReferences :: [DbExpression]
        , fieldRecord :: DbRecord
        , fieldType :: DbType
        }
    deriving (Show,Eq)

data DbSpec =
    DS { sname :: Name
       , sarity :: Int
       , stext :: String
       , sfile :: DbFile
       , sfunction :: DbFunction
       , sreferencedModule :: DbModule
       , sreturnType :: DbType
       , sguardType :: DbType
       , sarguments :: [DbSpecParameter]
       }
    deriving (Show,Eq)

data DbSpecParameter =
    DSP { spName :: [Name]
        , spIndex :: Int
        , spText :: String
        , spType :: DbType
        } 
    deriving (Show,Eq)

data DbType =
    DT { tname :: Name
       , tarity :: Int
       , texported :: Bool
       , topaque :: Bool
       , tbuiltin :: Bool
       , tfile :: [DbFile]
       , targuments :: [DbType]
       , tsubtypes :: [DbType]
       , tspecReferences :: [DbSpec]
       , tparameterReferences :: [DbType]
       }
    deriving (Show,Eq)
