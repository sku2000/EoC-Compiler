module TypeCheckCvar where

import ExplicateControl
import qualified Data.Map as Map

-- data Atm = Int Int | Var Var deriving(Show)
-- data Exp = Atom Atm | LRead | Neg Atm | Add Atm Atm deriving(Show)
-- data Stmt = Assign Var Exp deriving(Show)
-- data Tail = Return Exp | Seq Stmt Tail deriving(Show)
-- data Program info = Program info [(Label,Tail)] deriving(Show)


checkTail :: Num p => p -> Tail -> p
checkTail size (Seq stmt tail) = checkTail (size + 8) tail
checkTail size (Return e) = size

checkProgram :: Num info1 => Program info2 -> Program info1
checkProgram (Program info [(label, tail)]) = Program (checkTail 0 tail) [(label, tail)]
checkProgram _ = error "TypeCheckCvar error"