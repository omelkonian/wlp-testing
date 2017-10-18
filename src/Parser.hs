{-# LANGUAGE FlexibleContexts #-}

module Parser (programP) where

import Control.Monad
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as Tokens

import AST

-------------
-- Parsers --
-------------
test p = parse (p <* eof) ""
lexeme p = spaces *> p <* spaces
str = lexeme . string
commas l = l `sepBy1` string ", "
(<~) p s = p <* lexeme (string s)
(~>) s p = lexeme (string s) *> p
parens = Tokens.parens haskell
identifier = Tokens.identifier haskell
reserved = Tokens.reservedOp haskell
infix_ op f = Infix (reserved op >> return f) AssocLeft
prefix op f = Prefix (reserved op >> return f)

-- Programs
programP :: Parser Program
programP = (hoareProgramP <|> simpleProgramP) <* eof

-- hoareProgramP :: Parser Program
hoareProgramP = do
  pre <- "{" ~> exprP <~ "}"
  prog <- simpleProgramP
  post <- "{" ~> exprP <~ "}"
  return $ hoarify prog pre post

simpleProgramP :: Parser Program
simpleProgramP = do
  name <- programNameP <~ "("
  inputs <- commas nameP <~ "|"
  outputs <- commas nameP <~ ")"
  stmt <- stmtP
  return $ Prog name inputs outputs stmt

-- Statements
stmtP :: Parser Stmt
stmtP = buildExpressionParser table term <?> "expression"
  where table = [ [infix_ ";" Seq]]
        term = try varStmtP <|>
               try whileP <|>
               try iteP <|>
               try asgP <|>
               try assumeP <|>
               try assertP <|>
               skipP <?> "statement"

skipP :: Parser Stmt
skipP = Skip <$ str "skip"

assertP :: Parser Stmt
assertP = Assert <$> "assert" ~> exprP

assumeP :: Parser Stmt
assumeP = Assume <$> "assume" ~> exprP

asgP :: Parser Stmt
asgP = try arrayAsgP <|> simulAsgP

arrayAsgP :: Parser Stmt
arrayAsgP = do
  arr <- nameP <~ "["
  index <- exprP <~ "]"
  rhs <- ":=" ~> exprP
  return $ Asg [arr] [RepBy (Name arr) index rhs]

simulAsgP :: Parser Stmt
simulAsgP = do
  targets <- commas nameP <~ ":="
  rhs <- commas exprP
  return $ Asg targets rhs

iteP :: Parser Stmt
iteP = do
  expr <- "if" ~> exprP
  stmtT <- "then" ~> stmtP
  stmtF <- "else" ~> stmtP
  return $ Ite expr stmtT stmtF

whileP :: Parser Stmt
whileP = do
  invariant <- optionMaybe $ "{" ~> exprP <~ "}"
  expr <- "while" ~> exprP
  stmt <- "do" ~> stmtP <~ "end"
  return $ While invariant expr stmt

varStmtP :: Parser Stmt
varStmtP = do
  vars <- "var" ~> (nameP `sepBy1` char ',')
  stmt <- "in" ~> stmtP <~ "end"
  return $ VarStmt vars stmt

-- Expressions
exprP :: Parser Expr
exprP =
  try condP <|>
  try repbyP <|>
  buildExpressionParser table term <?> "expression"
  where table = [ [ infix_ "+" Plus, infix_ "-" Minus ]
                , [ infix_ "<" Lt, infix_ "=" Eq
                  , infix_ "<=" le_, infix_ ">" gt, infix_ ">=" ge
                  ]
                , [ prefix "~" Not]
                , [ infix_ "/\\" and_ ]
                , [ infix_ "\\/" or_ ]
                , [ infix_ "=>" Imply ]
                ]
        term = try (parens exprP) <|>
               try forallP <|>
               try arrayAccessP <|>
               primitiveP <?> "term"

repbyP :: Parser Expr
repbyP = do
  arr <- "[" ~> exprP
  index <- "|" ~> exprP
  rep <- "->" ~> exprP <~ "]"
  return $ RepBy arr index rep

condP :: Parser Expr
condP = do
  g <- "?" ~> exprP
  et <- "->" ~> exprP
  ef <- "|" ~> exprP
  return $ Cond g et ef

forallP :: Parser Expr
forallP = "(" ~> (forall <|> exists) <~ ")"
  where forall = do
         vs <- "forall" ~> commas nameP
         expr <- "::" ~> exprP
         return $ Forall vs expr
        exists = do
         vs <- "exists" ~> commas nameP
         expr <- "::" ~> exprP
         return $ Not (Forall vs (Not expr))

arrayAccessP :: Parser Expr
arrayAccessP = do
  name <- nameP
  expr <- "[" ~> exprP <~ "]"
  return $ ArrayAccess name expr

primitiveP :: Parser Expr
primitiveP = lexeme $
  (LitInt <$> numberP)  <|>
  (LitBool <$> boolP) <|>
  (Name <$> nameP)

programNameP :: Parser String
programNameP = many1 $ upper <|> char '_'

nameP :: Parser String
nameP = many1 $ lower <|> char '_'

numberP :: Parser Int
numberP = do
  n <- Tokens.integer haskell
  return $ fromInteger n

boolP :: Parser Bool
boolP = (True <$ str "true") <|> (False <$ str "false")

-- Types
typeP :: Parser Type
typeP = lexeme $
  (Prim <$> primitiveTypeP) <|>
  (Array <$> arrayTypeP)

primitiveTypeP :: Parser PrimitiveType
primitiveTypeP = lexeme $
  (Boolean <$ str "bool") <|>
  (Integer <$ str "int")

arrayTypeP :: Parser PrimitiveType
arrayTypeP = "[]" ~> primitiveTypeP
