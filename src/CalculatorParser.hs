{-# LANGUAGE QuasiQuotes #-}

module CalculatorParser where

import Calculator (Ast)
import Language.Haskell.TH
import Language.Haskell.TH.Quote

parseAst :: String -> Maybe (Ast Int)
parseAst = undefined



