module Expr
  (
    Expr, x, num, add, mul, sin, cos,
    showExpr, eval, readExpr, size,
    prop_ShowReadExpr, simplify,
    differentiate
  )
where

import Parsing hiding (digit)
import Data.Char(isDigit, isSpace)
import Test.QuickCheck
import Data.Maybe
import Prelude hiding (sin, cos)
import qualified Prelude as P

------A------
data Expr = Num Double
          | Var
          | Add Expr Expr
          | Mul Expr Expr
          | Sin Expr
          | Cos Expr
          deriving (Eq, Show)

x :: Expr
x = Var

num :: Double -> Expr
num n = Num n

mul :: Expr -> Expr -> Expr
mul e1 e2 = Mul e1 e2

add :: Expr -> Expr -> Expr
add e1 e2 = Add e1 e2

sin :: Expr -> Expr
sin e1 = Sin e1

cos :: Expr -> Expr
cos e1 = Cos e1

size :: Expr -> Int
size Var         = 0
size (Num n)     = 0
size (Sin e)     = 1 + size e
size (Cos e)     = 1 + size e
size (Add e1 e2) = 1 + size e1 + size e2
size (Mul e1 e2) = 1 + size e1 + size e2

------B------
-- instance Show Expr where
-- show = showExpr

showExpr :: Expr -> String
showExpr Var         = "x"
showExpr (Num n)     = show n
showExpr (Sin e)     = "sin " ++ showSinCos e
showExpr (Cos e)     = "cos " ++ showSinCos e
showExpr (Add e1 e2) = showExpr e1 ++ "+" ++ showExpr e2
showExpr (Mul e1 e2) = showFactor e1 ++ "*" ++ showFactor e2

showFactor :: Expr -> String
showFactor (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
showFactor e           = showExpr e

showSinCos :: Expr -> String
showSinCos Var       = "x"
showSinCos (Num n)   = show n
showSinCos (Add a b) = "(" ++ showExpr (Add a b) ++ ")"
showSinCos (Mul a b) = "(" ++ showFactor (Mul a b) ++ ")"
showSinCos e         = showExpr e

-- eval (Add (Add (Num 3) Var) (Sin Var))

------C------
eval :: Expr -> Double -> Double
eval Var value = value
eval (Num n) value     = n
eval (Add e1 e2) value = eval e1 value + eval e2 value
eval (Mul e1 e2) value = eval e1 value * eval e2 value
eval (Sin e) value     = P.sin (eval e value)
eval (Cos e) value     = P.cos (eval e value)

------D------
readExpr :: String -> Maybe Expr
readExpr s = let s' = filter (not.isSpace) s in case parse expr s' of
                                                  Just (e,"") -> Just e
                                                  _           -> Nothing

leftAssoc :: (t->t->t) -> Parser t -> Parser sep -> Parser t
leftAssoc op item sep = do is <- chain item sep
                           return (foldl1 op is)

digit :: Parser Char
digit = sat isDigit

number :: Parser Double
number = readsP

sinus :: Parser Expr
sinus = (char 's' *> char 'i' *> char 'n') *> factor

cosinus :: Parser Expr
cosinus = (char 'c' *> char 'o' *> char 's') *> factor

expr, term, factor :: Parser Expr
expr = leftAssoc Add term (char '+')
term = leftAssoc Mul factor (char '*')
factor = Num <$> number
         <|>  char '(' *> expr <* char ')'
         <|>  Sin <$> sinus
         <|>  Cos <$> cosinus
         <|>  return Var <* (char 'x')

prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr expr = (eval e1 0) `almostEqual` (eval expr 0)
                           where (Just e1) = readExpr $ showExpr expr

almostEqual :: Double -> Double -> Bool
almostEqual x y = (x - y) <= 1e-12

instance Arbitrary Expr where
  arbitrary = sized arbExpr

--sample arbExpr
arbExpr :: Int -> Gen Expr
arbExpr s =
  frequency [ (1, do n <- arbitrary
                     return (Num n))
            , (1, do return Var)
            , (s, do a <- arbExpr s'
                     return (Sin a))
            , (s, do b <- arbExpr s'
                     return (Cos b))
            , (s, do a <- arbExpr s'
                     b <- arbExpr s'
                     return (Add a b))
            , (s, do a <- arbExpr s'
                     b <- arbExpr s'
                     return (Mul a b))]
 where
  s' = s `div` 2

simplify :: Expr -> Expr
simplify Var     = Var
simplify (Num i) = (Num i)

simplify (Add e (Num 0))         = e
simplify (Add (Num 0) e)         = e
simplify (Add (Num n1) (Num n2)) = Num (n1 + n2)
simplify (Add e1 e2)
  | e1 == e1' && e2 == e2' = Add e1 e2
  | otherwise              = simplify (Add e1' e2')
     where e1' = simplify e1
           e2' = simplify e2

simplify (Mul _ (Num 0))       = Num 0
simplify (Mul (Num 0) _)       = Num 0
simplify (Mul (Num a) (Num b)) = Num (a * b)
simplify (Mul (Num 1) e)       = simplify e
simplify (Mul e (Num 1))       = simplify e
simplify (Mul e1 e2)
  | e1 == e1' && e2 == e2' = Mul e1 e2
  | otherwise              = simplify (Mul e1' e2')
     where e1' = simplify e1
           e2' = simplify e2

simplify (Sin (Num i)) = Num (P.sin i)
simplify (Sin e) = simplify' (Sin (simplify e))

simplify (Cos (Num i)) = Num (P.cos i)
simplify (Cos e) = simplify' (Cos (simplify e))

simplify' :: Expr -> Expr
simplify' (Cos (Num i)) = simplify (Cos (Num i))
simplify' (Sin (Num i)) = simplify (Sin (Num i))
simplify' e             = e

differentiate :: Expr -> Expr
differentiate = simplify . differentiateHelper

differentiateHelper :: Expr-> Expr
differentiateHelper (Num _)     = Num 0.0
differentiateHelper Var         = Num 1.0
differentiateHelper (Add e1 e2) = Add (differentiate e1)  (differentiate e2)
differentiateHelper (Mul e1 e2) = Add (Mul e1' e2) (Mul e1 e2')
                                    where e1' = differentiate e1
                                          e2' = differentiate e2
differentiateHelper (Cos (Num i)) = Num 0.0
differentiateHelper (Sin (Num i)) = Num 0.0
differentiateHelper (Sin e) = Cos e
differentiateHelper (Cos e) = Mul (Num (-1.0)) (Sin e)
