{-# LANGUAGE FlexibleContexts #-}

module Parser (programP, manyProgramP) where

import Control.Monad
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as Tokens

import AST

import Debug.Trace

-- | Useful marcros.
lexeme p = spaces *> p <* spaces
str = lexeme . string
commas l = l `sepBy` string ", "
(<~) p s = p <* lexeme (string s)
(~>) s p = lexeme (string s) *> p
parens = Tokens.parens haskell
identifier = Tokens.identifier haskell
reserved = Tokens.reservedOp haskell
infix_ op f = Infix (reserved op >> return f) AssocLeft
prefix op f = Prefix (reserved op >> return f)
postfix op f = Postfix (reserved op >> return f)

-- | Programs.
manyProgramP :: Parser [Program]
manyProgramP = many1 programP <* eof

programP :: Parser Program
programP = do
  pre <- "{" ~> exprP <~ "}"
  name <- nameP <~ "("
  inputs <- commas nameP <~ "|"
  outputs <- commas nameP <~ ")"
  stmt <- stmtP
  post <- "{" ~> exprP <~ "}"
  return $ hoarify $ Prog name inputs outputs stmt pre post

-- | Statements.
stmtP :: Parser Stmt
stmtP = buildExpressionParser table term
  where table = [ [infix_ ";" (<:>)] ]
        term = try (varStmtP <?> "var") <|>
               try (forP <?> "for") <|>
               try (whileP <?> "while") <|>
               try (iteP <?> "ite") <|>
               try (asgP <?> "asg") <|>
               try (assumeP <?> "assume") <|>
               try (assertP <?> "assert") <|>
               try (addP <?> "add") <|>
               try (subP <?> "sub") <|>
               try (incrementP <?> "inc") <|>
               try (decrementP <?> "dec") <|>
               skipP <?> "skip"

skipP :: Parser Stmt
skipP = Skip <$ string "skip"

incrementP :: Parser Stmt
incrementP = do
  x <- nameP <* string "++"
  return $ [x] .:= [n x .+ i 1]

decrementP :: Parser Stmt
decrementP = do
  x <- nameP <* string "--"
  return $ [x] .:= [n x .- i 1]

addP :: Parser Stmt
addP = do
  x <- nameP <~ "+="
  e <- exprP
  return $ [x] .:= [n x .+ e]

subP :: Parser Stmt
subP = do
  x <- nameP <~ "-="
  e <- exprP
  return $ [x] .:= [n x .- e]

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
  return $ [arr] .:= [RepBy (Name arr) index rhs]

simulAsgP :: Parser Stmt
simulAsgP = do
  targets <- commas nameP <~ ":="
  rhs <- commas exprP
  return $ targets .:= rhs

iteP :: Parser Stmt
iteP = do
  expr <- "if" ~> exprP
  stmtT <- "then" ~> stmtP
  stmtF <- "else" ~> stmtP <~ "fi"
  return $ Ite expr stmtT stmtF

whileP :: Parser Stmt
whileP = do
  invariant <- optionMaybe $ "{" ~> exprP <~ "}"
  expr <- "while" ~> exprP
  stmt <- "do" ~> stmtP <~ "end"
  return $ case invariant of
    i@(Just inv) -> Assert inv <:> While i expr (stmt <:> Assert inv)
    _ -> While Nothing expr stmt

forP :: Parser Stmt
forP = do
  x <- "for" ~> nameP
  low <- "in" ~> exprP
  high <- ".." ~> exprP
  stmt <- "do" ~> stmtP <~ "end"
  return $ VarStmt [x] $
    [x] .:= [low]
    <:> While Nothing (n x .< high .+ i 1) (stmt <:> [x] .:= [n x .+ i 1])

varStmtP :: Parser Stmt
varStmtP = do
  vars <- "var" ~> commas nameP
  stmt <- "in" ~> stmtP <~ "end"
  return $ VarStmt vars stmt

-- | Expressions.
exprP :: Parser Expr
exprP =
  try condP <|>
  try repbyP <|>
  try progCallP <|>
  buildExpressionParser table term <?> "expression"
  where table = [ [ infix_ "+" (.+), infix_ "-" (.-) ]
                , [ infix_ "=" (.=), infix_ "!=" (\e e' -> Not $ e .= e')
                  , infix_ "<" (.<), infix_ "<=" (.<=)
                  , infix_ ">" (.>), infix_ ">=" (.>=)
                  ]
                , [ prefix "~" Not]
                , [ infix_ "/\\" (/\) ]
                , [ infix_ "\\/" (\/) ]
                , [ infix_ "==>" (==>) ]
                ]
        term = try (parens exprP) <|>
               try rangeInInP <|>
               try rangeInExP <|>
               try rangeExInP <|>
               try rangeExExP <|>
               try arrayAccessP <|>
               try forallP <|>
               primitiveP <?> "term"

progCallP :: Parser Expr
progCallP = do
  prog <- nameP <~ "("
  inputs <- commas exprP <~ ")"
  return $ ProgCall prog inputs

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

rangeInInP :: Parser Expr
rangeInInP = do
  mid <- nameP <~ "in"
  low <- "[" ~> exprP
  high <- ".." ~> exprP <~ "]"
  return $ low .<= n mid /\ n mid .<= high

rangeExExP :: Parser Expr
rangeExExP = do
  mid <- nameP <~ "in"
  low <- "(" ~> exprP
  high <- ".." ~> exprP <~ ")"
  return $ low .< n mid /\ n mid .< high

rangeInExP :: Parser Expr
rangeInExP = do
  mid <- nameP <~ "in"
  low <- "[" ~> exprP
  high <- ".." ~> exprP <~ ")"
  return $ low .<= n mid /\ n mid .< high

rangeExInP :: Parser Expr
rangeExInP = do
  mid <- nameP <~ "in"
  low <- "(" ~> exprP
  high <- ".." ~> exprP <~ "]"
  return $ low .< n mid /\  n mid .<= high

forallP :: Parser Expr
forallP = "(" ~> (forall <|> exists) <~ ")"
  where forall = do
         vs <- "forall" ~> commas nameP
         expr <- "::" ~> exprP
         return $ Forall vs expr
        exists = do
         vs <- "exist" ~> commas nameP
         expr <- "::" ~> exprP
         return $ Exist vs expr

arrayAccessP :: Parser Expr
arrayAccessP = do
  name <- nameP <~ "["
  expr <- exprP <~ "]"
  return $ ArrayAccess name expr

primitiveP :: Parser Expr
primitiveP = lexeme $
  (LitInt <$> numberP)  <|>
  (LitBool <$> boolP) <|>
  (Name <$> nameP)

nameP :: Parser String
nameP = do
  base <- many1 $ letter <|> oneOf ['_', '$']
  suffix <- many digit
  return $ base ++ suffix

numberP :: Parser Int
numberP = do
  n <- Tokens.integer haskell
  return $ fromInteger n

boolP :: Parser Bool
boolP = (True <$ str "true") <|> (False <$ str "false")
