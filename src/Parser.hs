module Parser where
import Control.Applicative (Alternative (empty, some), (<|>))
import Witherable ( Filterable, mapMaybe )
import qualified Witherable as W
import GHC.TypeLits (charVal)
import Data.Char (isDigit)
import Data.Foldable (asum)

newtype ParserT s m a = Parser
  { parse :: s -> m (s, a)
  }

{-
Alternatively

type ParserT s m a = StateT s m a

-}

instance Functor m => Functor (ParserT s m) where
   fmap f p = Parser $ fmap (fmap f) . parse p

instance Filterable m => Filterable (ParserT s m) where
   mapMaybe f p = Parser $ mapMaybe (traverse f) . parse p

parseAll :: (Eq s, Monoid s, Filterable m) => ParserT s m a -> s -> m a
parseAll p = fmap snd . W.filter ((==) mempty . fst) . parse p

instance Monad m => Applicative (ParserT s m) where
   pure a = Parser $ \s -> pure (s, a)
   p <*> q = Parser $ \s -> do
       (s1, a1) <- parse p s
       (s2, a2) <- parse q s1
       pure (s2, a1 a2)

instance (Monad m, Alternative m) => Alternative (ParserT s m) where
   empty = Parser $ const empty
   p <|> q = Parser $ \s -> parse p s <|> parse q s

choice :: (Monad m , Alternative m) => [ParserT s m a] -> ParserT s m a
choice = asum

type Parser = ParserT String Maybe

anyChar :: Parser Char
anyChar = Parser p where
  p [] = Nothing
  p (c:s) = Just (s, c)

char :: Char -> Parser Char
char c = W.filter (c ==) anyChar

newline :: Parser Char
newline = char '\n'

space :: Parser Char
space = char ' '

spaces :: Parser String
spaces = some space

string :: String -> Parser String
string = traverse char

digit :: Parser Char
digit = W.filter isDigit anyChar



