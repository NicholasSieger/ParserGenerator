--reads backus naur form grammar and generates a parser 
--for the given grammar
module BNF_Reader (
)where

import LilParser
import Data.Char
import Control.Monad
import Control.Applicative
--reads in list of syntax rules

--generate -- takes BNF grammar and produces a parser 
--for said grammar
--otuput parser produces syntax tree for a given input

--create list of rules
syntax -> rule list (key-val mapping between rule_names and expressions)
rule -> expression (list of possible parsers)
expression -> option list 
option -> term list
term -> literal or rule_name
literal -> string of any kind
rule_name -> letters

--given list of rules, create parser
--specifically, create list of parsers
--and for each rule, call parsers from the list as the need arises

data Literal = Value String | Rule String
data SynTree a = Leaf a | Node String [SynTree a]

expression :: Parser [[Literal]]
expression = do {

list :: Parser ([Literal])
list = do { t<-term;
            whitespace;
            ts<-list;
            return (t:ts)} <|> fmap [] $ term

whitespace :: Parser String
whitespace = many (satisfy isSpace)

term :: Parser Literal
term = fmap Value $ parens ('"','"') text <|> 
       fmap Rule $ parens ('<','>') text


text :: Parser String
text = some (satisfy isAlpha) 

