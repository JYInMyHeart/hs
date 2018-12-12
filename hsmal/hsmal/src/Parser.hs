{-# LANGUAGE FlexibleContexts #-}

module Parser where

import           Data.Functor.Identity
import           Data.List
import           Text.Parsec
import qualified Text.Parsec.Token     as P
import           Context
import           Syntax

type Parser a = ParsecT String Context Identity a

untypedDef :: P.LanguageDef st
untypedDef =
  P.LanguageDef { P.commentStart    = ""
                , P.commentEnd      = ""
                , P.commentLine     = ""
                , P.nestedComments  = True
                , P.identStart      = letter
                , P.identLetter     = alphaNum
                , P.opStart         = letter
                , P.opLetter        = alphaNum
                , P.reservedOpNames = [ "lambda","if","then","else","true","false","Bool","succ","pred","iszero" ,"Nat","Unit","unit"]
                , P.reservedNames   = ["lambda","if","then","else","true","false","Bool","succ","pred","iszero","Nat","Unit","unit"]
                , P.caseSensitive  = True
                }

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser untypedDef

dot :: ParsecT String u Identity String
dot        = P.dot        lexer
colon :: ParsecT String u Identity String
colon = P.colon lexer
identifier :: ParsecT String u Identity String
identifier = P.identifier lexer

parens :: ParsecT String u Identity a -> ParsecT String u Identity a
parens     = P.parens     lexer

reserved :: String -> ParsecT String u Identity ()
reserved   = P.reserved   lexer
reservedOp :: String -> ParsecT String u Identity ()
reservedOp = P.reservedOp lexer
getVarIndex :: (Monad m, Eq a) => a -> [(a,b)] -> m Int
getVarIndex var ctx =
  case findIndex ((== var) . fst) ctx of
    Just i  -> return i
    Nothing -> error "Unbound variable name"

parseVar :: Parser Term
parseVar =
  do var <- identifier
     ctx <- getState
     idx <- getVarIndex var ctx
     return $ TermVar idx (length ctx)

parseAbs :: Parser Term
parseAbs =
  do reservedOp "lambda"
     var   <- identifier
     tyVar <- parseTypeAnnotation
     dot
     ctx   <- getState
     setState $ addBinding (var, VarBinding tyVar) ctx
     term  <- parseTerm
     setState ctx
     return $ TermAbs var tyVar term


parseUnit :: Parser Term
parseUnit = reserved "unit" >> return TermUnit

parseTrue :: Parser Term
parseTrue = reserved "true" >> return TermTrue
parseFalse :: Parser Term
parseFalse = reserved "false" >> return TermFalse


parseIf :: Parser Term
parseIf =
  do reservedOp "if"
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
parseSucc =
  do reservedOp "succ"
     many space
     t <- parseTerm
     return $ TermSucc t
     
parsePred :: Parser Term
parsePred =
  do reservedOp "pred"
     many space
     t <- parseTerm
     return $ TermPred t
     
parseIsZero :: Parser Term
parseIsZero =
  do reservedOp "zero?"
     many space
     t <- parseTerm
     return $ TermIsZero t

parseTypeBool :: Parser Type
parseTypeBool = reserved "Bool" >> return TypeBool

parseTypeUnit :: Parser Type
parseTypeUnit = reserved "Unit" >> return TypeUnit

parseTypeNat :: Parser Type
parseTypeNat = reserved "Nat" >> return TypeNat

parseTypeArrow :: Parser Type
parseTypeArrow =
  do tyT1 <- parseTypes
     many space
     string "->"
     many space
     tyT2 <- parseType
     return $ TypeArrow tyT1 tyT2

parseGType :: Parser Type
parseGType = reserved "A" >> return TypeT

parseTypes :: Parser Type
parseTypes =
  try parseTypeBool  <|> parseTypeNat <|> parseTypeUnit <|> parseGType

parseType :: Parser Type
parseType =
  try parseTypeArrow <|> parseTypes <|> parens parseType 

parseTypeAnnotation :: Parser Type
parseTypeAnnotation =
  do colon
     parseType
parseTerm :: Parser Term
parseTerm =
  chainl1 (
    parseZero   <|>
    parseSucc   <|>
    parsePred   <|>
    parseIsZero <|> 
    parseTrue   <|>
    parseFalse  <|>
    parseUnit   <|>
    parseIf     <|>
    parseAbs    <|> 
    parseVar    <|> 
    parens parseTerm) 
          (return TermApp)
