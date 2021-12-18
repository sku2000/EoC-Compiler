module RvarInterp where

import Rvar
    ( Program(Program),
      Exp(..),
      Var,
      Primop(Add, LRead, Neg),
      program )
import qualified Data.Map as Map

-- data Exp = LInt Int 
--     | Prim Primop [Exp] 
--     | Var Var
--     | Let Var Exp Exp deriving(Show)
    
-- data Program info = Program info Exp deriving(Show)

isUndif :: [Char] -> Maybe p -> p
isUndif var (Just x) = x
isUndif var Nothing = error ("undefined variable" ++ var)

interpExp :: Map.Map Var (IO Int) -> Exp -> IO Int
interpExp env (LInt x) = return x
interpExp env (Var var) = isUndif var (Map.lookup var env)
interpExp env (Let var exp1 exp2) = interpExp (Map.insert var (interpExp env exp1) env) exp2
interpExp env (Prim LRead []) = getLine >>= \x -> return (read x)
interpExp env (Prim Neg [exp]) = interpExp env exp >>= \x -> return (-x)
interpExp env (Prim Add [exp1, exp2]) = do
    a <- interpExp env exp1 
    b <- interpExp env exp2
    return (a + b)
interpExp _ _ = error "Undefined behavior"


interp :: Program info -> IO Int
interp (Program _ exp) = interpExp Map.empty exp

