{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Calculator where

import Data.Eq.Deriving
import Data.Fix (Fix (Fix))
import Data.Functor.Foldable (cata)
import Text.Show.Deriving

data AstF a b
  = ValueF !a
  | AddF !b !b
  | MultipleF !b !b
  deriving (Eq, Show, Functor)

type Ast a = Fix (AstF a)

deriveShow1 ''AstF
deriveEq1 ''AstF

value :: Int -> Ast Int
value = Fix . ValueF

add :: Ast Int -> Ast Int -> Ast Int
add a1 a2 = Fix (AddF a1 a2)

multiply :: Ast Int -> Ast Int -> Ast Int
multiply a1 a2 = Fix (MultipleF a1 a2)

evaluate :: Ast Int -> Int
evaluate = cata algebra
 where
  algebra :: AstF Int Int -> Int
  algebra (ValueF a) = a
  algebra (AddF a1 a2) = a1 + a2
  algebra (MultipleF a1 a2) = a1 * a2

pretty :: Ast Int -> String
pretty = cata algebra
 where
  algebra :: AstF Int String -> String
  algebra (ValueF a) = show a
  algebra (AddF a1 a2) = "(" ++ a1 ++ "+" ++ a2 ++ ")"
  algebra (MultipleF a1 a2) = "(" ++ a1 ++ "*" ++ a2 ++ ")"

simplify :: Ast Int -> Ast Int
simplify = cata algebra
 where
  algebra :: AstF Int (Ast Int) -> Ast Int
  algebra (AddF (Fix (ValueF 0)) a) = a
  algebra (AddF a (Fix (ValueF 0))) = a
  algebra (MultipleF (Fix (ValueF 1)) a) = a
  algebra (MultipleF a (Fix (ValueF 1))) = a
  algebra a = Fix a

