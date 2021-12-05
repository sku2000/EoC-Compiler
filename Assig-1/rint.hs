import Lparser(token, symb, Parser, chainl1, (+++), sat, apply, space)
import Data.Char

-- The concrete syntax of RInt
-- exp ::= int | (read) | (- exp) | (+ exp exp)
-- RInt ::= exp

data Primop = LRead | Neg | Add deriving(Show)

data Exp = LInt Int 
    | Prim Primop [Exp] deriving(Show)

data Program info = Program info Exp deriving(Show)

expr :: Parser Exp
expr = rread +++ neg +++ add +++ (nat >>= \x -> return (LInt x)) 

rread :: Parser Exp
rread =  symb "(" >>  symb "read" >> symb ")" >> return (Prim LRead [])

neg :: Parser Exp
neg =  symb "(" >>  symb "-" >> expr >>= \x -> symb ")" >> return (Prim Neg [x])

add :: Parser Exp
add = do 
    symb "(" 
    symb "+"
    x <- expr
    space 
    y <- expr
    symb ")" 
    return (Prim Add [x, y])

nat :: Parser Int
nat = space >> ((sat isDigit >>= \x -> return (digitToInt x - digitToInt '0')) `chainl1` (return op))
        where m `op` n = 10*m + n

program :: info -> Parser (Program info)
program info = expr >>= \x -> return (Program info x)

parser :: String -> Program [Char]
parser xs =  case apply (program "") xs of 
        [(a, "")] -> a
        [(_, es)] -> error ("Parser did not consume entire stream : " ++ es)
        _ -> error "Parser error."


-- data Primop = LRead | Neg | Add deriving(Show)

-- data Exp = LInt Int 
--     | Prim Primop [Exp] deriving(Show)

interpExp :: Exp -> IO Int
interpExp (LInt x) = return x
interpExp (Prim LRead []) = getLine >>= \x -> return (read x)
interpExp (Prim Neg [exp]) = interpExp exp >>= \x -> return (-x)
interpExp (Prim Add [exp1, exp2]) = do
    a <- interpExp exp1 
    b <- (interpExp exp2)
    return (a + b)


interp :: Program [Char] -> IO Int
interp (Program _ e) = interpExp e


