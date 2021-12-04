import Lparser(token, symb, Parser, chainl1, (+++), sat, apply, space, many)
import Data.Char ( isAlphaNum, isDigit, digitToInt, isLetter )

-- exp ::= int | (read) | (- exp) | (+ exp exp)
--         | var | (let ([var exp]) exp)
-- RVar ::= exp

data Primop = LRead | Neg | Add deriving(Show)

type Var = String

data Exp = LInt Int 
    | Prim Primop [Exp] 
    | Var Var
    | Let Var Exp Exp deriving(Show)
    
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

-- isSym :: [Char] -> Parser Char
-- isSym xs = sat isAlphaNum +++ p xs
--     where p (x:xs) = sat (== x) +++ p xs
--           p [] = sat (const False)

var :: Parser Exp
var = space >> (sat isLetter >>= sym)
        where p (x:xs) = sat (== x) +++ p xs
              p [] = sat (const False)
              p2 xs = sat isAlphaNum +++ p xs            
              sym x = many (p2 "~`!@#$%^&*_-;:") >>= \xs -> return (Var (x:xs))