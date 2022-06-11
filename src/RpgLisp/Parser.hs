module RpgLisp.Parser (parseSingleExpr, parseManyExprWithLines) where

import Data.Void
import Control.Applicative

import Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec as M

import RpgLisp.Grammar
import Text.Megaparsec (errorBundlePretty)
import Data.Maybe
import Data.Functor

type Parser = M.Parsec Void String

parseManyExprWithLines :: String -> Either String [(ParsedExpr, Int)]
parseManyExprWithLines str = case M.parse (many expressionWithLine <* M.eof) "" str of
  Left a -> Left $ errorBundlePretty a
  Right expr -> Right expr

parseSingleExpr :: String -> Either String ParsedExpr
parseSingleExpr str = case M.parse (expression <* M.eof) "" str of
  Left a -> Left $ errorBundlePretty a
  Right expr -> Right expr

expressionWithLine :: Parser (ParsedExpr, Int)
expressionWithLine = do
  line <- M.unPos . M.sourceLine <$> M.getSourcePos
  expr <- expression
  pure (expr, line)

expression :: Parser ParsedExpr
expression = lexeme $ sc *> M.choice (map M.try [
  intExpr,
  stringExpr,
  boolExpr,
  nilExpr,
  listExpr,
  atomExpr])

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") (L.skipBlockComment "#|" "|#")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = M.between (lexeme $ symbol "(") (lexeme $ symbol ")")

quotedParens :: Parser a -> Parser a
quotedParens = M.between (lexeme $ symbol "'(") (lexeme $ symbol ")")

stringExpr :: Parser ParsedExpr
stringExpr = PExprString <$> (char '\"' *> M.manyTill L.charLiteral (char '\"'))

boolExpr :: Parser ParsedExpr
boolExpr = PExprBool <$> (
  M.try (symbol "#t" $> True)
  <|> M.try (symbol "#f" $> False))

nilExpr :: Parser ParsedExpr
nilExpr = symbol "#nil" $> PExprNil

intExpr :: Parser ParsedExpr
intExpr = do
  sign <- optional $ char '-'
  let coeff = if isJust sign then (-1) else 1
  decimal <- lexeme L.decimal
  pure $ PExprInt $ decimal * coeff

atomExpr :: Parser ParsedExpr
atomExpr = M.try atomExpr'
       <|> M.try (fmap quoteExpr $ char '\'' *> atomExpr')

atomExpr' :: Parser ParsedExpr
atomExpr' = PExprAtom <$> M.some (M.noneOf [' ', '\'', '\n', '\r', '\t', '\f', '\v',  '#', '(', ')'])

listExpr :: Parser ParsedExpr
listExpr = M.try (PExprList <$> parens (M.sepBy expression sc))
       <|> M.try (quoteExpr . PExprList <$> quotedParens (M.sepBy expression sc))

quoteExpr :: ParsedExpr -> ParsedExpr
quoteExpr expr = PExprList [PExprAtom "quote", expr]
