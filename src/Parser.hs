module Parser(parseString) where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import AST

languageDef = emptyDef {
                        Token.identStart = letter,
                        Token.identLetter = alphaNum,
                        Token.reservedOpNames = ["⊢","|-", "¬", "!", "and", "∧", "or", "∨", "->", "→"]
                       }

lexer = Token.makeTokenParser languageDef
identifier = Token.identifier lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
whiteSpace = Token.whiteSpace lexer
commaSep = Token.commaSep lexer

mainParser :: Parser Sequent
mainParser = do
             whiteSpace
             antecedent <- commaSep formula
             reservedOp "⊢" <|> reservedOp "|-"
             succedent <- commaSep formula
             return (Turnstile antecedent succedent)

formula :: Parser Formula
formula = buildExpressionParser operators term

operators = [
              [Prefix (reservedOp "!" >> return Not), Prefix (reservedOp "¬" >> return Not)],
              [Infix (reservedOp "and" >> return And) AssocRight, Infix (reservedOp "∧" >> return And) AssocRight],
              [Infix (reservedOp "or" >> return Or) AssocRight, Infix (reservedOp "∨" >> return Or) AssocRight],
              [Infix (reservedOp "->" >> return Impl) AssocRight, Infix (reservedOp "→" >> return Impl) AssocRight]
            ]

term :: Parser Formula
term = parens formula <|> fmap Var identifier

-------Start Parse
parseString :: String -> Sequent
parseString str = case parse mainParser "" str of
                    Left e -> error $ show e
                    Right r -> r
