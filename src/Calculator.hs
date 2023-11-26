{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Calculator where

import Data.Eq.Deriving
import Data.Fix (Fix (Fix))
import Data.Functor.Foldable (cata)
import Text.Show.Deriving

data AstF a
  = ValueF Int
  | AddF a a
  | MultipleF a a
  deriving (Eq, Functor)

type Ast = Fix AstF

deriveShow1 ''AstF
deriveEq1 ''AstF

-- instance Show (Fix AstF) where
-- show = pretty

-- instance Eq (Fix AstF) where
-- (==) (Fix a1) (Fix a2) = a1 == a2

value :: Int -> Ast
value = Fix . ValueF

add :: Ast -> Ast -> Ast
add a1 a2 = Fix (AddF a1 a2)

multiply :: Ast -> Ast -> Ast
multiply a1 a2 = Fix (MultipleF a1 a2)

evaluateF :: AstF Int -> Int
evaluateF (ValueF a) = a
evaluateF (AddF a1 a2) = a1 + a2
evaluateF (MultipleF a1 a2) = a1 * a2

evaluate :: Ast -> Int
evaluate = cata evaluateF

prettyF :: AstF String -> String
prettyF (ValueF a) = show a
prettyF (AddF a1 a2) = "(" ++ a1 ++ "+" ++ a2 ++ ")"
prettyF (MultipleF a1 a2) = "(" ++ a1 ++ "*" ++ a2 ++ ")"

pretty :: Ast -> String
pretty = cata prettyF

simplifyF :: AstF Ast -> Ast
simplifyF (AddF (Fix (ValueF 0)) a) = a
simplifyF (AddF a (Fix (ValueF 0))) = a
simplifyF (MultipleF (Fix (ValueF 1)) a) = a
simplifyF (MultipleF a (Fix (ValueF 1))) = a
simplifyF a = Fix a

simplify :: Ast -> Ast
simplify = cata simplifyF
