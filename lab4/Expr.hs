
module Expr where
import Parsing hiding (digit)
import Data.Char(isDigit, isSpace)
import Control.Monad(forever)



data Expr = Num Double
          | Var
          | Add Expr Expr
          | Mul Expr Expr
          | Sin Expr
          | Cos Expr
          deriving (Eq,Show)

--x :: Expr
--x = Var

--num :: Double -> Expr
--num n= Num n

--mul :: Expr -> Expr -> Expr
--mul e1 e2 = Mul e1 e2

--add :: Expr -> Expr -> Expr
--add e1 e2 = Add e1 e2

--sin' :: Expr -> Expr
--sin' e1 = Sin e1

--cos' :: Expr -> Expr
--cos' e1 = Cos e1


--instance Show Expr where
 -- show = showExpr



showExpr :: Expr -> String
showExpr (Num n) = show n
showExpr Var = "x"
showExpr (Sin e)= "sin" ++ (showSinCos e)
showExpr (Cos e)= "cos" ++ (showSinCos e)
showExpr (Add e1 e2) = showExpr e1 ++ "+" ++ showExpr e2
showExpr (Mul e1 e2) = showFactor e1 ++ "*" ++ showFactor e2


showFactor (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++")"
showFactor e = showExpr e

showSinCos :: Expr->String
showSinCos Var ="x"
showSinCos (Num n) = show n
showSinCos (Add a b) = "(" ++ showExpr(Add a b) ++ ")"
showSinCos (Mul a b) = "(" ++ showFactor(Mul a b) ++ ")"
showSinCos x         = showExpr x


--eval (Add (Add (Num 3) Var) (Sin Var))
eval :: Expr -> Double -> Double
eval (Num n) value = n
eval (Var) value = value
eval (Add e1 e2) value = eval e1 value + eval e2 value
eval (Mul e1 e2) value = eval e1 value * eval e2 value
eval (Sin e) value = sin (eval e value)
eval (Cos e) value = cos (eval e value)



-----------------------------------------------------------------------------------------------------------

--readExpr :: String -> Maybe Expr
--readExpr s = let s' = filter (not.isSpace) s
        --     in case parse expr s' of
           --          Just (e,"") -> Just e

leftAssoc :: (t->t->t) -> Parser t -> Parser sep -> Parser t
leftAssoc op item sep = do is <- chain item sep
                           return (foldl1 op is)
            --         _           -> Nothing

digit :: Parser Char
digit = sat isDigit

number :: Parser Double
number = read <$> oneOrMore digit



expr, term, factor :: Parser Expr
expr = leftAssoc Add term (char '+')
term = leftAssoc Mul factor (char '*')
factor = Num <$> number
         <|>  (char '(' *> expr <* char ')')



