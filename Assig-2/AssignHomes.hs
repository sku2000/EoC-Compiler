module AssignHomes where

import SelectInstructions
import qualified Data.Map as Map

-- data Reg = RSP | RBP | RAX | RBX | RCX | RDX | RSI | RDI 
--     | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 deriving(Show)
-- type Label = String
-- data Arg = Imm Int
--     | Reg Reg
--     | Deref Reg Int
--     | X86Var String deriving(Show)
-- data Instr = Addq Arg Arg | Subq Arg Arg | Negq Arg 
--     | Movq Arg Arg | Movabsq Arg Arg | Callq Label Int
--     | Retq | Pushq Arg | Popq Arg | Jmp Label deriving(Show)
-- data Block binfo = Block binfo [Instr] deriving(Show)
-- data X86Program pinfo binfo = X86Program pinfo [(Label, Block binfo)] deriving(Show)


assignHomeBlock rbp rsp (instr:instrs) env = 


assignHomeProgram (X86Program pinfo [(label, block)]) = X86Program pinfo [(label, (assignHomeBlock pinfo 0 block Map.empty))]

