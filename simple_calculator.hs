import LilParser
import Control.Applicative


data Expr = 
     -- Pow Expr Expr
     Mul Expr Expr
    |Div Expr Expr
    |Add Expr Expr
    |Sub Expr Expr
    |Lit Int 
    deriving (Eq,Ord,Show)


eval :: Expr -> Int
-- eval (Pow a b) = power (eval a) (eval b)
eval (Mul a b) = (eval a) * (eval b)
eval (Div a b) = if ansB /= 0 then div (eval a) ansB 
                            else error "divide by zero"
                            where ansB = eval b
eval (Add a b) = (eval a) + (eval b)
eval (Sub a b) = (eval a) - (eval b)
eval (Lit n) = n

expr :: Parser Expr
expr = chain term addop (Lit 0)

term :: Parser Expr
term = chain factor mulop (Lit 1)

factor :: Parser Expr
factor = parens ('(',')') expr <|> literal

literal :: Parser Expr
literal = do {n <- number; return (Lit n)}

addop :: Parser (Expr -> Expr -> Expr)
addop = (operator "+" (Add)) <|> (operator "-" (Sub))

mulop :: Parser (Expr -> Expr -> Expr)
mulop = (operator "*" (Mul)) <|> (operator "/" (Div))

-- expop :: Parser (Expr -> Expr -> Expr)
-- expop = operator "**" (Exp)

run :: String -> Expr
run = runParser expr

main :: IO ()
main =  do
  putStr "> "
  a <- getLine
  print $ run a
