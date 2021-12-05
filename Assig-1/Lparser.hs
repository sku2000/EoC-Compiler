module Lparser where

import GHC.Unicode (isSpace)
newtype Parser a = Parser (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p

item :: Parser Char 
item = Parser (\cs -> case cs of
                "" -> []
                (x:xs) -> [(x, xs)])

instance Functor Parser where
    fmap fun (Parser p) = Parser (\cs -> concat [[(fun a, cs')] | 
                                        (a, cs') <- p cs])

instance Applicative Parser where
    pure a = Parser (\cs -> [(a, cs)])
    (<*>) (Parser pf) (Parser p) = Parser (\cs -> concat [[(fun b, cs'')] |
                                            (fun, cs') <- pf cs, (b, cs'') <- p cs'])

instance Monad Parser where
    return a = pure  a
    p >>= f = Parser (\cs -> concat [parse (f a) cs' | 
                                (a, cs') <- parse p cs])

class Monad m => MonadZero m where
    zero :: m a

class MonadZero m => MonadPlus m where
    plus :: m a -> m a -> m a

instance MonadZero Parser where
    zero = Parser (const [])

instance MonadPlus Parser where
    p `plus` q = Parser (\cs -> parse p cs ++ parse q cs)

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse (p `plus` q) cs of
                            [] -> []
                            (x:xs) -> [x])

sat :: (Char -> Bool) -> Parser Char 
sat p = do
    c <- item
    if p c then return c
            else zero
char :: Char -> Parser Char 
char c = sat (\x -> c == x)

string :: String -> Parser String
string "" = return ""
string (x:xs) = char x >> string xs >> return (x:xs)

many1 :: Parser a -> Parser [a]
many1 p = do 
    x <- p
    xs <- many p
    return (x:xs)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do 
    a <- p
    as <- many (sep >> p)
    return (a:as)

sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) +++ return []

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = p >>= rest
                where
                    rest a = (do
                        f <- op
                        b <- p 
                        rest (f a b))
                        +++ return a

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) +++ return a

space :: Parser String
space = many (sat isSpace)

token :: Parser a -> Parser a
token p = p >>= \a -> space >> return a

symb :: String -> Parser String
symb cs = token (string cs)

apply :: Parser a -> String -> [(a, String)]
apply p = parse (space >> p)