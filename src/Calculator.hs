{-# LANGUAGE DeriveFunctor #-}
module Calculator where
import Data.Fix ( Fix(Fix, unFix) )
import Data.Text.Prettyprint.Doc (Doc)
import Data.Functor.Foldable (cata)

data AstF a
  = ValueF Int
  | AddF a a
  | MultipleF a a deriving (Functor)

type Ast = Fix AstF

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
