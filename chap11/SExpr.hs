{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

-- runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH" == Just ("ABC","dEfgH")
-- runParser (zeroOrMore (satisfy isUpper)) "AcBCdEfgH" == Just ("A","cBCdEfgH")
-- runParser (zeroOrMore (satisfy isUpper)) "cBCdEfgH" == Just ("","cBCdEfgH")
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

-- runParser (oneOrMore (satisfy isUpper)) "cBCdEfgH" == Nothing
-- runParser (oneOrMore (satisfy isUpper)) "ABcBCdEfgH" == Just ("AB","cBCdEfgH")
oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p 

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

-- runParser spaces "34" == Just ("","34")
-- runParser spaces "   34" == Just ("   ","34")
spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

-- runParser ident "foobar baz" == Just ("foobar"," baz")
-- runParser ident "foob33c" == Just ("foob33c","")
-- runParser ident "" == Nothing
-- runParser ident " adf" ==  Nothing
ident :: Parser String
ident = (++) <$> oneOrMore(satisfy isAlpha) <*> (zeroOrMore $ satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

parseAtom :: Parser Atom
parseAtom =  spaces *> ((I <$> ident) <|> (N <$> posInt)) <* spaces

parseSExpr :: Parser SExpr
parseSExpr = A <$> (char '(' *> parseAtom <* char ')')
