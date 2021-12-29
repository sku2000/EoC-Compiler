module ExplicateControl where

import qualified Rvar

type Var = String

data Atm = Int Int | Var Var deriving(Eq, Show)

data Exp = Atom Atm | LRead | Neg Atm | Add Atm Atm deriving(Show)

data Stmt = Assign Var Exp deriving(Show)

data Tail = Return Exp | Seq Stmt Tail deriving(Show)

data Program info = Program info [(String,Tail)] deriving(Show)

-- data Exp = LInt Int 
--     | Prim Primop [Exp] 
--     | Var Var
--     | Let Var Exp Exp deriving(Show)

explicateVarInt :: Rvar.Exp -> Atm
explicateVarInt (Rvar.Var v) = Var v
explicateVarInt (Rvar.LInt i) = Int i
explicateVarInt _ = error "Undefined behavior"

explicateTail :: Rvar.Exp -> Tail
explicateTail exp = case exp of
    (Rvar.Var v) -> Return (Atom (Var v))
    (Rvar.LInt i) -> Return (Atom (Int i))
    (Rvar.Let var exp1 exp2) -> explicateAssign exp1 var (explicateTail exp2)
    (Rvar.Prim Rvar.LRead []) -> Return LRead
    (Rvar.Prim Rvar.Neg [x]) -> Return (Neg (explicateVarInt x))
    (Rvar.Prim Rvar.Add [x, y] ) -> Return (Add (explicateVarInt x) (explicateVarInt y))
    _ -> error "Undefined behavior"

explicateAssign :: Rvar.Exp -> Rvar.Var -> Tail -> Tail
explicateAssign exp var cont = case exp of
    (Rvar.Var v) -> Seq (Assign var (Atom (Var v))) cont
    (Rvar.LInt i) -> Seq (Assign var (Atom (Int i))) cont
    (Rvar.Let y rhs body) -> explicateAssign rhs y (explicateAssign body var cont)
    (Rvar.Prim Rvar.LRead []) -> Seq (Assign var LRead) cont
    (Rvar.Prim Rvar.Neg [x]) -> Seq (Assign var (Neg (explicateVarInt x))) cont
    (Rvar.Prim Rvar.Add [x, y] ) -> Seq (Assign var (Add (explicateVarInt x) (explicateVarInt y))) cont
    _ -> error "Undefined behavior"

explicateControlProgram :: Rvar.Program info -> Program info
explicateControlProgram (Rvar.Program info exp) = Program info [("main", explicateTail exp)] 