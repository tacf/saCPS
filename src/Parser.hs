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

cpsDef :: LanguageDef st
cpsDef = LanguageDef
           {commentStart   = "/*"
           , commentEnd     = "*/"
           , commentLine    = "//"
          , nestedComments = True
           , identStart     = letter
           , identLetter    = alphaNum
           , opStart        = opLetter laDef
           , opLetter       = oneOf "+-"
           , reservedOpNames= [":=","\\","."]
           , reservedNames  = ["k.k","if"]
           , caseSensitive  = True
           }


la :: TokenParser st
la = makeTokenParser laDef

cps :: TokenParser st
cps = makeTokenParser cpsDef

number  :: Parser Int
number  = do{ ds <- many1 digit
            ; return (read ds)
            }

parseVector :: Char -> Parser a -> Parser [a]
parseVector c p = do { l <- sepBy p (skipMany1 (space <|> char c))
                     ; return l
                     }

parseE :: Parser E
parseE = do { c <- number
            ; return $ CNST c
            }
	 <|> do { l <- (identifier la)
            ; return $ VAR l
            }


parseB :: Parser B
parseB = do { c <- number
            ; return $ LCNST c
            }
         <|>
         do { l <- (identifier cps)
            ; return $ ID l
            }

parseInv :: Parser (Label,[E])
parseInv = do { l <- (identifier la)
              ; vec <- (parens la) (parseVector ',' parseE)
              ; return $ (l,vec)
              }

parseLInv :: Parser (Label,[B])
parseLInv = do { l <- (identifier cps)
               ; vec <- parseVector ' ' (do{(reservedOp cps)"\\";(reserved cps) "k.k" ;parseB})
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
            ; exp <- try (do { e <- parseE
                             ; char ';'
                             ; (skipMany1 (space <|> char '\n'))
                             ; s1 <- parseS
                             ; return $ ATRB x e s1
                             } )
                          <|>
                          do { l <- (identifier la)
                             ; vec <- (parens la) (parseVector ',' parseE)
                             ; char ';'
                             ; (skipMany1 (space <|> char '\n'))
                             ; s1 <- parseS
                             ; return $ INV x l vec s1
                             }
            ; return exp
            }


parseM :: Parser M
parseM = do { char '('
             ; m <-
                  try(
                  do { (reserved cps) "if"
                     ; b  <- (parens cps) (do{(reservedOp cps) "\\"; (reserved cps) "k.k" ; parseB })
                     ; m0 <- parseM
                     ; m1 <- parseM
                     ; char ')'
                     ; return $ LIF b m0 m1
                     }
                     )
                  <|>
                  do { (reservedOp cps) "\\"
                     ; (reserved cps) "k.k"
                     ;  retOrBndr <- ((parens cps)
                                            (do{ (reservedOp cps) "\\"
                                               ; (reserved cps) "k.k"
                                               ; b <- parseB
                                               ; char ')'
                                               ; return $ LRET b
                                               })
                                        )
                                    <|>
                                    do { b <- parseB
                                       ; char ')'
                                       ; r <- (parens cps)
                                               (do { (reservedOp cps) "\\"
                                                   ; x <- (identifier cps)
                                                   ; char '.'
                                                   ; m <- parseM
                                                   ; return $ BNDR x b m
                                                   })

                                       ; return r
                                       }
                    ; return retOrBndr
                    }
                  <|>
                  do { (i,l) <- parseLInv
                     ; char ')'
                     ; inv <-
                       (parens cps) (do { x <- (identifier cps)
                                        ; m <- parseM
                                        ; return $ LINV x i l m
                                        })
                     ; return inv
                     }
             ; return m
             }


parseProc :: Parser Proc
parseProc = do { l <- (identifier la)
               ; (reservedOp la) ":="
               ; (reserved la) "proc"
               ; vec  <- (parens la) (parseVector ',' (identifier la))
               ; body <- (braces la) parseS
               ; return $ PROC l vec body
               }

parseLProc :: Parser LProc
parseLProc = do { l <- (identifier cps)
               ; (reservedOp cps) ":="
               ; proc <- (parens cps) (
                    do { (reservedOp cps) "\\"
                       ; (reserved la) "proc"
                       ; vec  <- (parens cps) (parseVector ',' (identifier cps))
                       ; (reservedOp cps) "."
                       ; body <- parseM
                       ; return $ LPROC l vec body
                       })
               ; return proc
               }


parseProgram :: Parser Program
parseProgram = do { l <- sepBy parseProc (skipMany space)
                 ; return l
                 }

parseLProgram :: Parser LProgram
parseLProgram = do { l <- sepBy parseLProc (skipMany space)
                 ; return l
                 }

