module LilParser (
Parser(..),
runParser,
satisfy,
char,
string,
chain,
token,
spaces,
reserved,
parens,
operator,
number
)where

import Data.Char
import Control.Monad
import Control.Applicative

newtype Parser a = Parser {parse :: String -> [(a,String)]}

--runs the parser on a string
--currently only accepts unqiue parses
runParser :: Parser a -> String -> a
runParser p s = 
      case parse p s of 
         [(ans,[])] -> ans; 
         [(_,str)]  -> error ("Input not consumed" ++ str);
          _          -> error "Parser fail";

--allows the application of functions on the result of a parse
instance Functor Parser where
    fmap f (Parser cs) = Parser (\s -> [(f a,s) | (a,s) <- cs s])

--allows lifting of curried functions to operate on parses    
instance Applicative Parser where
   pure x = Parser (\s -> [(x,s)])
   (Parser f) <*> (Parser x) = 
            Parser (\s -> [(f a,s2) | (f,s1)<-f s, (a,s2)<-x s1])

--allows efficient chaining of parses
instance Monad Parser where
    return = pure
    p >>= f = Parser $ \s-> concatMap (\(a,s1)->parse (f a) s1)  $ parse p s

--creates a monoid out of the parser
--allows multiple parses of same string
instance MonadPlus Parser where
   mzero = Parser (\s -> [])
   mplus p q = Parser $ \s->parse p s ++ parse q s

--allows for switching between parsers if first parser fails
instance Alternative Parser where
   empty = mzero
   p <|> q = Parser $ \s-> case parse p s of
                            [] -> parse q s
                            res -> res
--gets the first character from the parse string
item :: Parser Char
item = Parser $ \s -> case s of 
                       []    -> []
                       (c:cs)-> [(c,cs)]

--checks if the first char of the parse string satisfies a test
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = item >>= \c -> if f c then pure c else mzero

--only accepts a particular character
char :: Char -> Parser Char
char c = satisfy (c == )

--accepts one of a list of characters
oneOf :: String -> Parser Char
oneOf s = satisfy (flip elem s)

--parses the following pattern: val fun val fun ... val
--requires an alternative if the main parse fails
chain :: Parser a -> Parser (a->a->a) -> a -> Parser a
chain p op a = (chain_h p op) <|> return a

--helper for chain-- parses val fun val fun .. val
chain_h :: Parser a -> Parser (a->a->a) -> Parser a
chain_h p op = do {a <- p; rest a} 
           where rest a = do {f<- op; 
                              b<- p; 
                              rest (f a b)} 
                              <|> return a
--parses a specific string
string :: String -> Parser String
string [] = return []
string (c:cs) = do {char c; string cs; return (c:cs)}

spaces :: Parser Char
spaces =  oneOf " \n\r"

--parses anything with whitespace after it
token :: Parser a -> Parser a
token p = do {a <- p; 
              spaces; 
              return a}

--turns a specific string into a token
reserved :: String -> Parser String
reserved s = token (string s)

--returns a function if a specific string is parsed
operator :: String -> (a->a->a) -> Parser (a->a->a)
operator s f = reserved s >> return f 

--gets a parse from inside parentheses
parens:: (Char,Char)->Parser a -> Parser a
parens (s,e) p = do {char s; a <- p; char e; return a}

--helper for number--returns a list of digits and a sign
number_h :: Parser (Int,[Int])
number_h = do {s<- fmap (\c->(-1)) (char '-') <|> return 1;
               ns<-some (satisfy isDigit);
               return (s , map digitToInt ns)} 

--parses a signed integer
number :: Parser Int
number = fmap (\(s,ds)-> s * (foldl (\a x->10*a + x) 0 ds)) 
          (token number_h)  
