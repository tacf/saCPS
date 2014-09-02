{-
  Module Compiler

  Description: Module that defines compiler functions
-}

module Compiler(compile, compileL) where

import Languages
import Utilities

-- Imperative Compiler
invPara :: [Label] -> [E] -> S -> S
invPara [] [] s = s
invPara [] (e:es) s = s -- Malformed invocation 
invPara (l:ls) [] s = s -- Malformed invocation
invPara (l:ls) (e:es) s = (ATRB l e (invPara ls es s))

subs :: S -> Label -> S -> S
subs (RET e) x s          = ATRB x e s
subs (ATRB y e sp) x s    = ATRB y e (subs sp x s)
subs (INV y f vec sp) x s = INV y f vec (subs sp x s)
subs (IF e s1 s2) x s     = IF e (subs s1 x s) (subs s2 x s)

subsF :: Label -> S -> S -> [Label] -> S
subsF _ _ (RET e) v          = RET e
subsF f s (ATRB x e sp) v    = ATRB x e (subsF f s sp v)
subsF f s (INV x g vec sp) v | f == g = 
                                invPara v vec (subs s x (subsF f s sp v))
                             | otherwise = INV x g vec (subsF f s sp v)
subsF f s (IF e s1 s2) v     = IF e (subsF f s s1 v) (subsF f s s2 v) 

compile :: Program -> Program
compile p = compileAux ( getProgramRefList p ) p

compileAux :: [Label] -> Program -> Program
compileAux [] p = p
compileAux (l:ls) p = let 
        replaceIn (Just x) =
          map (\(PROC lp vec s) 
                -> PROC lp vec (subsF l (getProcBody x) s (getProcPara x))) p
        replaceIn Nothing  = p
        in compileAux ls (replaceIn (findProc p l))



-- Functional Compiler
invLPara :: [Label] -> [B] -> M -> M
invLPara [] [] m         = m
invLPara [] (b:bs) m     = m -- Malformed invocation 
invLPara (l:ls) [] m     = m -- Malformed invocation
invLPara (l:ls) (b:bs) m = (BNDR l b (invLPara ls bs m))

subsL :: M -> Label -> M -> M
subsL (LRET b) x m            = BNDR x b m
subsL (BNDR y b mp) x m       = BNDR y b (subsL mp x m)
subsL (LINV y f vec mp) x m   = LINV y f vec (subsL mp x m)
subsL (LIF b m1 m2) x m       = LIF b (subsL m1 x m) (subsL m2 x m)

subsLF :: Label -> M -> M -> [Label] -> M
subsLF _ _ (LRET b) v           = LRET b
subsLF f m (BNDR x b mp) v      = BNDR x b (subsLF f m mp v)
subsLF f m (LINV x g vec mp) v  | f == g = 
                                    invLPara v vec (subsL m x (subsLF f m mp v))
                                | otherwise = LINV x g vec (subsLF f m mp v)
subsLF f m (LIF b m1 m2) v      = LIF b (subsLF f m m1 v) (subsLF f m m2 v) 

compileL:: LProgram -> LProgram
compileL p = compileLAux ( getLProgramRefList p ) p

compileLAux :: [Label] -> LProgram -> LProgram
compileLAux [] p = p
compileLAux (l:ls) p = let 
        replaceIn (Just x) = 
         map (\(LPROC lp vec s) -> 
          LPROC lp vec (subsLF l (getLProcBody x) s (getLProcPara x))) p
        replaceIn Nothing  = compileLAux ls p
        in compileLAux ls (replaceIn (findLProc p l))