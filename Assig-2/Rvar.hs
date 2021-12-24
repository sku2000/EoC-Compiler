module Rvar where

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

parser :: info -> Parser (Program info)
parser info = expr >>= \x -> return (Program info x)

program :: String -> Program [Char]
program xs =  case apply (parser "") xs of 
        [(a, "")] -> a
        [(_, es)] -> error ("Parser did not consume entire stream : " ++ es)
        _ -> error "Parser error."

expr :: Parser Exp
expr = rread +++ neg +++ add +++ (nat >>= \x -> return (LInt x)) +++ (var >>= \x -> return (Var x)) +++ rlet

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
nat = space >> ((sat isDigit >>= \x -> return (digitToInt x - digitToInt '0')) `chainl1` return op)
        where m `op` n = 10*m + n

var :: Parser String
var = space >> (sat isLetter >>= sym)
        where p (x:xs) = sat (== x) +++ p xs
              p [] = sat (const False)
              p2 xs = sat isAlphaNum +++ p xs            
              sym x = many (p2 "~`!@#$%^&*_-;:") >>= \xs -> return (x:xs)

rlet :: Parser Exp
rlet = do
    symb "("
    symb "let"
    symb "("
    symb "["
    v <- var
    space
    exp1 <- expr
    space
    symb "]"
    symb ")"
    l <- Let v exp1 <$> expr
    symb ")"
    return l

