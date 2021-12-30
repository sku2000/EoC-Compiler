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

rcoExp :: (Num a, Show a) => Exp -> a -> ((Exp, a) -> Exp) -> Exp
rcoExp (LInt x) varId cont =cont (LInt x, varId)
rcoExp (Var v) varId cont = cont (Var v, varId)
rcoExp (Prim LRead []) varId cont = cont (Prim LRead [], varId)
rcoExp (Let var exp1 exp2) varId cont = rcoExp exp1 varId (\(x, vId) -> Let var x (rcoExp exp2 vId cont))
rcoExp (Prim Neg [exp]) varId cont = handlingComplexExpressions exp varId (\(x, vId) -> cont (Prim Neg [x], vId))
rcoExp (Prim Add [exp1, exp2]) varId cont = handlingComplexExpressions exp1 varId (\(x, vId) -> 
    handlingComplexExpressions exp2 vId (\(y, vId2) -> cont (Prim Add [x, y], vId)))
rcoExp _ _ _ = error "Undefined behavior"

handlingComplexExpressions :: (Num a, Show a) => Exp -> a -> ((Exp, a) -> Exp) -> Exp
handlingComplexExpressions exp varId cont = case exp of
    (LInt x) -> cont (LInt x, varId)
    (Var v) -> cont (Var v, varId)
    (Prim LRead []) -> cont (Prim LRead [], varId)
    (Let var exp1 exp2) -> handlingComplexExpressions exp1 varId 
                                (\(x, vId) -> Let var x (handlingComplexExpressions exp2 vId cont))
    (Prim Neg [e]) -> let newId = gensym varId in 
        handlingComplexExpressions e (varId + 1) 
            (\(x, vId)-> Let newId (Prim Neg [x]) (cont (Var newId, vId)))
    (Prim Add [exp1, exp2]) -> let newId = gensym varId in handlingComplexExpressions exp1 (varId + 1)
        (\(x, vId) -> handlingComplexExpressions exp2 vId 
            (\(y, vId2) -> Let newId (Prim Add [x, y]) (cont (Var newId, vId2))))
    _ -> error "RemoveComplexOpera error"

removeComplexOperands :: Program info -> Program info
removeComplexOperands (Program info exp) = Program info (rcoExp exp 0 fst)