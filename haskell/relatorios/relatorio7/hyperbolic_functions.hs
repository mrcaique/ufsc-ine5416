-- HyperbolicFunctions module
--
-- This module implements some basic trigonometrics
-- operations like hyperbolic sine, hyperbolic
-- cosine, hyperbolic tangent and hyperbolic cotangent
module HyperbolicFunctions (HFunct(Sinh, Cosh, Tanh, Coth)) where

data HFunct = Sinh Float
    | Cosh Float
    | Tanh Float
    | Coth Float
    deriving Show

-- e (mathematical constant)
e = 1 + sum([1/product[1..n] | n <- take 1000 [1..]])

value :: HFunct -> Float
value(Sinh x) = (e**x - e**(-x))/2
value(Cosh x) = (e**x + e**(-x))/2
value(Tanh x) = (value(Sinh x))/(value(Cosh x))
value(Coth x) = (value(Cosh x))/(value(Sinh x))
