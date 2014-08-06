module Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Languages
import Converter
                   
laDef :: LanguageDef st
laDef = LanguageDef 
           { commentStart   = "/*"
           , commentEnd     = "*/"
           , commentLine    = "//"
           , nestedComments = True
           , identStart     = letter
           , identLetter    = alphaNum
           , opStart        = opLetter laDef
           , opLetter       = oneOf "+-"
           , reservedOpNames= [":="]
           , reservedNames  = ["proc","if","else","ret"]
           , caseSensitive  = True
           }

la :: TokenParser st
la = makeTokenParser laDef


number  :: Parser Int
number  = do{ ds <- many1 digit
            ; return (read ds)
            }

parseVectorE :: Parser [E]
parseVectorE = do { l <- sepBy parseE (skipMany1 (space <|> char ','))
                 ; return l
                 }

parseVector :: Parser [Label]
parseVector = do { l <- sepBy (identifier la) (skipMany1 (space <|> char ','))
                 ; return l
                 }

parseE :: Parser E
parseE = do { c <- number; 
	      return $ CNST c } 
	<|> do { l <- (identifier la);
		 return $ VAR l} 

parseInv :: Parser (Label,[E])
parseInv = do { l <- (identifier la) 
              ; vec <- (parens la) parseVectorE
              ; return $ (l,vec)
              }

parseS :: Parser S
parseS = do { (reserved la) "ret"
            ; e <- (parens la) parseE
            ; return $ RET e
            }
         <|> 
         do { (reserved la) "if"
            ; e <- (parens la) parseE
            ; s0 <- (braces la) parseS
            ; (reserved la) "else"
            ; s1 <- (braces la) parseS
            ; return $ IF e s0 s1
            }
         <|>
         do { x <- (identifier la);
            ; (reservedOp la) ":="
            ; exp <- try (do { 
                  e <- parseE
                ; char ';'
                ; s1 <- parseS
                ; return $ ATRB x e s1
                } )
                <|>
                do {
                  l <- (identifier la)
                ; vec <- (parens la) parseVectorE
                ; char ';'
                ; s1 <- parseS
                ; return $ INV x l vec s1
                }
            ; return exp
            }

parseProc :: Parser Proc
parseProc = do { l <- (identifier la)
               ; (reservedOp la) ":="
               ; (reserved la) "proc"
               ; vec  <- (parens la) parseVector
               ; body <- (braces la) parseS
               ; return $ PROC l vec body
               }
               
               
parseProgram :: Parser Program
parseProgram = do { l <- sepBy parseProc (skipMany space)
                 ; return l
                 }



