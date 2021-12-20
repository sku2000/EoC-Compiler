module RvarUniquifyPass where

import Rvar
    ( Program(Program),
      Exp(..),
      Var,
      Primop(Add, LRead, Neg),
      program )
import qualified Data.Map as Map

isUndif :: [Char] -> Maybe p -> p
isUndif var (Just x) = x
isUndif var Nothing = error ("undefined variable" ++ var)

gensym :: Show a => [Char] -> a -> [Char]
gensym var id = var ++ show id

-- data Exp = LInt Int 
--     | Prim Primop [Exp] 
--     | Var Var
--     | Let Var Exp Exp deriving(Show)
    
-- data Program info = Program info Exp deriving(Show)

uniquifyExp :: (Show b, Num b) => Map.Map Var Var -> Exp -> b -> (Exp, b)
uniquifyExp env exp varId = case exp of
    (LInt x) -> (LInt x, varId)
    (Var var) -> (Var (isUndif var (Map.lookup var env)), varId)
    (Let var exp1 exp2) -> let newVar = gensym var varId in
                               let (e1, newId1) = uniquifyExp env exp1 (varId + 1) in
                                   let (e2, newId2) = uniquifyExp (Map.insert var newVar env) exp2 newId1 in
                                       (Let newVar e1 e2, newId2)
    (Prim LRead []) -> (Prim LRead [], varId)
    (Prim Neg [exp]) -> let (e1, newId) = uniquifyExp env exp varId in
                            (Prim Neg [e1], newId)
    (Prim Add [exp1, exp2]) -> let (e1, newId1) = uniquifyExp env exp1 varId in
                                   let (e2, newId2) = uniquifyExp env exp2 newId1 in
                                       (Prim Add [e1, e2], newId2)
    _ -> error "Undefined behavior"

uniquify :: Program info -> Program info
uniquify (Program info exp) = let (exp1, _) = uniquifyExp Map.empty exp 0 in
                                  Program info exp1
