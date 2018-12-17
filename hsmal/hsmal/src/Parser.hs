{-# LANGUAGE FlexibleContexts #-}

module Parser where

import Context
import Data.Functor.Identity
import Data.List
import Syntax
import Text.Parsec
import qualified Text.Parsec.Token as P

type Parser a = ParsecT String Context Identity a

untypedDef :: P.LanguageDef st
untypedDef = P.LanguageDef
  { P.commentStart    = ""
  , P.commentEnd      = ""
  , P.commentLine     = "--"
  , P.nestedComments  = True
  , P.identStart      = letter
  , P.identLetter     = alphaNum
  , P.opStart         = letter
  , P.opLetter        = alphaNum
  , P.reservedOpNames = [ "\\"
                        , "if"
                        , "then"
                        , "else"
                        , "true"
                        , "false"
                        , "Bool"
                        , "succ"
                        , "pred"
                        , "iszero"
                        , "Nat"
                        , "Unit"
                        , "unit"
                        , "_"
                        , "as"
                        , ";"
                        , "]"
                        , "["
                        , "\""
                        , "String"
                        ]
  , P.reservedNames   = [ "\\"
                        , "if"
                        , "then"
                        , "else"
                        , "true"
                        , "false"
                        , "Bool"
                        , "succ"
                        , "pred"
                        , "iszero"
                        , "Nat"
                        , "Unit"
                        , "unit"
                        , "_"
                        , "as"
                        , ";"
                        , "["
                        , "]"
                        , "\""
                        , "String"
                        ]
  , P.caseSensitive   = True
  }

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser untypedDef

dot :: ParsecT String u Identity String
dot = P.dot lexer

colon :: ParsecT String u Identity String
colon = P.colon lexer

identifier :: ParsecT String u Identity String
identifier = P.identifier lexer

parens :: ParsecT String u Identity a -> ParsecT String u Identity a
parens = P.parens lexer

reserved :: String -> ParsecT String u Identity ()
reserved = P.reserved lexer

reservedOp :: String -> ParsecT String u Identity ()
reservedOp = P.reservedOp lexer

getVarIndex :: (Monad m, Eq a) => a -> [(a, b)] -> m Int
getVarIndex var ctx = case findIndex ((== var) . fst) ctx of
  Just i  -> return i
  Nothing -> error "Unbound variable name"

parseAs :: Parser Term
parseAs = do
  as <- identifier
  many space
  reserved "as"
  many space
  ty <- parseType
  many space
  ctx <- getState
  setState $ addBinding (as, VarBinding ty) ctx
  return $ TermAs as ty

parseVar :: Parser Term
parseVar = do
  var <- identifier
  ctx <- getState
  idx <- getVarIndex var ctx
  return $ TermVar idx (length ctx)

parseAbs :: Parser Term
parseAbs =
  try
      (do
        reservedOp "\\"
        var   <- identifier
        tyVar <- parseTypeAnnotation
        dot
        ctx <- getState
        setState $ addBinding (var, VarBinding tyVar) ctx
        term <- parseTerm
        setState ctx
        return $ TermAbs var tyVar term
      )
    <|> do
          reservedOp "\\"
          reserved "_"
          tyVar <- parseTypeAnnotation
          dot
          term <- parseTerm
          return $ TermAbs "_" tyVar term

parseUnit :: Parser Term
parseUnit = reserved "unit" >> return TermUnit

parseTrue :: Parser Term
parseTrue = reserved "true" >> return TermTrue

parseFalse :: Parser Term
parseFalse = reserved "false" >> return TermFalse

parseStr :: Parser Term
parseStr = do
  many space
  string "\""
  s <- many (oneOf ['a' .. 'z'])
  string "\""
  many space
  return $ TermString s
ch = P.charLiteral lexer

parseIf :: Parser Term
parseIf = do
  reservedOp "if"
  many space
  predicate <- parseTerm
  many space
  reservedOp "then"
  many space
  consequent <- parseTerm
  many space
  reservedOp "else"
  many space
  antecedent <- parseTerm
  return $ TermIf predicate consequent antecedent

parseZero :: Parser Term
parseZero = reserved "0" >> return TermZero

parseSucc :: Parser Term
parseSucc = do
  reservedOp "succ"
  many space
  t <- parseTerm
  return $ TermSucc t

parsePred :: Parser Term
parsePred = do
  reservedOp "pred"
  many space
  t <- parseTerm
  return $ TermPred t

parseIsZero :: Parser Term
parseIsZero = do
  reservedOp "zero?"
  many space
  t <- parseTerm
  return $ TermIsZero t

parseTypeBool :: Parser Type
parseTypeBool = reserved "Bool" >> return TypeBool

parseTypeUnit :: Parser Type
parseTypeUnit = reserved "Unit" >> return TypeUnit

parseTypeNat :: Parser Type
parseTypeNat = reserved "Nat" >> return TypeNat

parseStrType :: Parser Type
parseStrType = reserved "String" >> return TypeString

parseTypeArrow :: Parser Type
parseTypeArrow = do
  tyT1 <- parseTypes
  many space
  string "->"
  many space
  tyT2 <- parseType
  return $ TypeArrow tyT1 tyT2

parseGType :: Parser Type
parseGType = oneOf ['A' .. 'Z'] >> return TypeT

parseTypes :: Parser Type
parseTypes =
  try parseTypeBool
    <|> parseTypeNat
    <|> parseTypeUnit
    <|> parseStrType
    <|> parseGType
    <|> parseAsType

parseAsType :: Parser Type
parseAsType = do
  var <- identifier
  ctx <- getState
  case getBinding var ctx of
    Just (VarBinding a) -> return a
    Nothing             -> error "undefined as type"


parseType :: Parser Type
parseType = try parseTypeArrow <|> parseTypes <|> parens parseType

parseTypeAnnotation :: Parser Type
parseTypeAnnotation = do
  colon
  parseType

parseList :: Parser Term
parseList = do
  t  <- parseTerm
  ts <- many parseSeTerm
  return $ TermList (t : ts)

parseSeTerm :: Parser Term
parseSeTerm = do
  reserved ";"
  parseTerm

parseTerm :: Parser Term
parseTerm = chainl1
  (   parseZero
  <|> parseSucc
  <|> parsePred
  <|> parseStr
  <|> parseIsZero
  <|> parseTrue
  <|> parseFalse
  <|> parseUnit
  <|> try parseIf
  <|> try parseAbs
  <|> try parseAs
  <|> try parseVar
  <|>
    --  parseList <|>
      parens parseTerm
  )
  (return TermApp)

