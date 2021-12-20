module RemoveComplexOpera where

import Rvar
    ( Program(Program),
      Exp(..),
      Var,
      Primop(Add, LRead, Neg),
      program )
import qualified Data.Map as Map

-- data Primop = LRead | Neg | Add deriving(Show)

-- data Exp = LInt Int 
--     | Prim Primop [Exp] 
--     | Var Var
--     | Let Var Exp Exp deriving(Show)

-- atm ::= (Int int) | (Var var)
-- exp ::= atm | (Prim read ())
--      | (Prim - (atm)) | (Prim + (atm atm))
--      | (Let var exp exp)
-- R ::= (Program ’() exp)

gensym :: Show a => a -> [Char]
gensym id = "_A" ++ show id

isAtm :: Exp -> Bool
isAtm (LInt x) = True 
isAtm (Var v) = True 
isAtm _ = False 

rcoExp :: (Num b, Show b) => Exp -> b -> (Exp, b)
rcoExp (LInt x) varId = (LInt x, varId)
rcoExp (Var v) varId = (Var v, varId)
rcoExp (Let var exp1 exp2) varId = let (e1, newId1) = rcoExp exp1 varId
                                       (e2, newId2) = rcoExp exp2 newId1 
                                   in  (Let var e1 e2, newId2)
rcoExp (Prim LRead []) varId = (Prim LRead [], varId)
rcoExp (Prim Neg [exp]) varId = if isAtm exp 
                                then (Prim Neg [exp], varId)
                                else let newId = gensym varId  
                                         (e1, newId1) = rcoExp exp (varId + 1)
                                     in  (Let newId e1 (Prim Neg [Var newId]), newId1)
rcoExp (Prim Add [exp1, exp2]) varId = case (isAtm exp1, isAtm exp2) of
    (True, True) -> (Prim Add [exp1, exp2], varId)
    (False, True) -> let newVar = gensym varId  
                         (e1, newId1) = rcoExp exp1 (varId + 1)
                     in  (Let newVar e1 (Prim Add [Var newVar, exp2]), newId1)
    (True, False) -> let newVar = gensym varId 
                         (e2, newId2) = rcoExp exp2 (varId + 1)
                     in  (Let newVar e2 (Prim Add [exp1, Var newVar]), newId2)
    (False, False) -> let newVar1 = gensym varId
                          (e1, newId1) = rcoExp exp1 (varId + 1)
                          newVar2 = gensym newId1
                          (e2, newId2) = rcoExp exp2 (newId1 + 1)
                      in (Let newVar1 e1 (Let newVar2 e2 (Prim Add [Var newVar1, Var newVar2])), newId2)
rcoExp _ _ = error "Undefined behavior"
                                       

removeComplexOperands :: Program info -> Program info
removeComplexOperands (Program info exp) = let (e, _) = rcoExp exp 0 in Program info e