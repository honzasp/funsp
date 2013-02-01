{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Krunimir.Parser
( Krunimir.Parser.parse
)
where
import Text.Parsec
import Control.Applicative ((<$>), (<$), (<*), (<*>))
import Krunimir.Ast

parse :: String -> String -> Either ParseError Program
parse filename txt =
  Text.Parsec.parse program filename txt

program = spaces >> many topStmt <* eof
topStmt = 
  TopDefine <$> try define <|>
  TopStmt <$> stmt

define = do
  _ <- string "define" >> many1 space
  name <- many1 lower
  params <- parens $ sepBy identifier comma
  stmts <- braces $ many stmt
  return $ Define name params stmts

stmt =
  try repeatStmt <|>
  try ifStmt <|>
  try splitStmt <|>
  try funStmt <?>
  "statement"

funStmt = do
  name <- identifier
  args <- parens $ sepBy expr comma
  case name of
    "forward" -> fun1 ForwardStmt "forward" args
    "left"    -> fun1 LeftStmt "left" args
    "right"   -> fun1 RightStmt "right" args
    "pen"     -> fun1 PenStmt "pen" args
    "color"
      | [r,g,b] <- args -> return $ ColorStmt r g b
      | otherwise       -> parserFail $ "color takes 3 arguments, got " ++ show (length args)
    _ -> return $ FunStmt name args

fun1 con name args 
  | [arg] <- args = return $ con arg
  | otherwise     = parserFail $ name ++ " takes 1 argument, got " ++ show (length args)

repeatStmt = do
  keyword "repeat"
  times <- parens expr
  stmts <- braces $ many stmt
  return $ RepeatStmt times stmts

ifStmt = do
  keyword "if"
  cond <- parens expr
  stmts <- braces $ many stmt
  return $ IfStmt cond stmts

splitStmt = do
  keyword "split"
  stmts <- braces $ many stmt
  return $ SplitStmt stmts

expr = addExpr <?> "expression"

addExpr = chainl1 mulExpr (addOp <* spaces)
mulExpr = chainl1 aExpr (mulOp <* spaces)

addOp = 
  Binop AddOp <$ char '+' <|>
  Binop SubOp <$ char '-' 

mulOp =
  Binop MulOp <$ char '*' <|>
  Binop DivOp <$ char '/' 

aExpr = litExpr <|> varExpr <|> parens expr
varExpr = Variable <$> identifier
litExpr = Literal <$> integer

integer = read <$> ((++) <$> option "" (string "-") <*> many1 digit <* spaces)
identifier = many1 lower <* spaces

keyword s = string s >> notFollowedBy alphaNum >> spaces

lparen = char '(' >> spaces
rparen = char ')' >> spaces
lbrace = char '{' >> spaces
rbrace = char '}' >> spaces
parens = between lparen rparen
braces = between lbrace rbrace
comma = char ',' >> spaces
