{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances, ExistentialQuantification #-}
import Parser hiding (Env)
import Text.Parsec (parse, ParseError)
import qualified Sq

    
instance Show Value where
    show (Module m) = Sq.mname m
    show (Function f) = Sq.fname f
    show (Seq vs)   = show vs

instance Sq.Named Value where
    name (Module m) = Sq.mname m
    name (Function f) = Sq.fname f

instance Wrap Sq.DbModule where
    wrap = Module
    unwrap (Module m) = m

instance Wrap a => Wrap [a] where
    wrap = Seq . map wrap
    unwrap (Seq xs) = map unwrap xs


type Env = [(Id, Value)]

-- data AValue = forall a. AValue (Value a) (TTyp a)
{-
data Value a where
    VModule :: Sq.DbModule -> Value Sq.DbModule

class Type a where
    theType :: TTyp a

instance Type Sq.DbModule where
    theType = TTMod
-}
{-
parseEval :: Wrap a => String -> Maybe [a]
parseEval s = case parse query "" s of
                Right t -> do {q ::: _ <- check t []; return $ eval q []}
                Left e  -> Nothing
-}

eval :: Wrap a =>  Env -> TQuery [a] -> [a]
eval env (TBind m (TF (TVarExpr x) body)) = concat [eval ((x, wrap a) :env) body | a <- as]
    where
      as = eval env m

{-
eval :: Wrap a => TQuery [a] -> Env -> [a]
eval (TBind m (TF (TVarExpr x) body)) env = concat [eval body ((x, wrap a):env) | a <- as]
    where
      as = eval m env
-}
{-
eval (VarExpr v) cont = readVar v cont
eval (AppExpr Functions (VarExpr v)) cont = let (Module m) = readVar v cont in
                                            Seq $ map Function (Sq.functions m)
eval (AppExpr Name (VarExpr v)) cont = String . Sq.name $ readVar v cont
eval Modules _cont = Seq $ map Module Sq.modules
eval (Return e) cont = Seq $ [eval e cont]
eval (StringLit s) _cont = String s
eval (Relation p1 rel p2) cont = Bool $ evalRel p1' rel p2'
    where p1' = eval p1 cont
          p2' = eval p2 cont
eval (Guard rel) cont = if p then Seq [Unit] else Seq []
    where Bool p = eval rel cont

evalRel p1 Eq p2 = p1 == p2
-- evalRel p1 Gt p2 = p1 <  p2

readVar :: Var -> Env -> Value
readVar v cont = fromJust $ lookup v cont
-}
