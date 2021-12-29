module SelectInstructions where

import ExplicateControl

data Reg = RSP | RBP | RAX | RBX | RCX | RDX | RSI | RDI 
    | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 deriving(Show)

type Label = String

data Arg = Imm Int
    | Reg Reg
    | Deref Reg Int
    | X86Var String deriving(Show)

data Instr = Addq Arg Arg | Subq Arg Arg | Negq Arg 
    | Movq Arg Arg | Movabsq Arg Arg | Callq Label Int
    | Retq | Pushq Arg | Popq Arg | Jmp Label deriving(Show)

data Block binfo = Block binfo [Instr] deriving(Show)

data X86Program pinfo binfo = X86Program pinfo [(Label, Block binfo)] deriving(Show)

-- data Atm = Int Int | Var Var deriving(Show)
-- data Exp = Atom Atm | LRead | Neg Atm | Add Atm Atm deriving(Show)
-- data Stmt = Assign Var Exp deriving(Show)
-- data Tail = Return Exp | Seq Stmt Tail deriving(Show)
-- data Program info = Program info [(Label,Tail)] deriving(Show)

selectInstAtom :: Atm -> Arg
selectInstAtom (Int i) = Imm i
selectInstAtom (Var v) = X86Var v

selectInstStmt (Assign v exp) = case exp of 
    (Atom atm) -> [Movq (selectInstAtom atm) (X86Var v)] 
    LRead -> [Callq "read_int" 0, Movq (Reg RAX) (X86Var v)]
    (Neg (Int i)) -> [Movq (Imm i) (X86Var v), Negq (X86Var v)]
    (Neg (Var v1)) -> if v1 == v then [Negq (X86Var v)] else [Movq (X86Var v1) (X86Var v), Negq (X86Var v)] 
    (Add (Int x) (Int y)) -> [Movq (Imm x) (X86Var v), Addq (Imm y) (X86Var v)] 
    (Add (Int x) (Var v2)) -> if v2 == v then [Addq (Imm x) (X86Var v)] 
                                else [Movq (Imm x) (X86Var v), Addq (X86Var v2) (X86Var v)]
    (Add (Var v1) (Int x)) -> if v1 == v then [Addq (Imm x) (X86Var v)] 
                                else [Movq (Imm x) (X86Var v), Addq (X86Var v1) (X86Var v)]
    (Add (Var v1) (Var v2)) -> if v1 /= v && v2 /= v 
                               then [Movq (X86Var v1) (X86Var v), Addq (X86Var v2) (X86Var v)]
                               else [Addq (X86Var (if v1 /= v then v1 else v2)) (X86Var v)]

selectInstTail (Return exp) = case exp of
    (Atom atm) -> [Movq (selectInstAtom atm) (Reg RAX)]
    LRead -> [Callq "read_int" 0]
    (Neg atm) -> [Movq (selectInstAtom atm) (Reg RAX), Negq (Reg RAX)]
    (Add atm1 atm2) -> [Movq (selectInstAtom atm1) (Reg RAX), Addq (selectInstAtom atm2) (Reg RAX)]
selectInstTail (Seq stmt tail) = selectInstStmt stmt ++ selectInstTail tail


selectInst (Program info ((label, tail):ps)) =  X86Program info [(label,Block "start" (selectInstTail  tail))] 
selectInst (Program info []) = error "error"