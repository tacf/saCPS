{-
  Module Compiler

  Description: Module that defines compiler functions
-}

module Compiler(compile, compileL) where

import Languages
import Utilities

-- Imperative Compiler
subs :: S -> Label -> S -> S
subs (RET e) x s          = ATRB x e s
subs (ATRB y e sp) x s    = ATRB y e (subs sp x s)
subs (INV y f vec sp) x s = INV y f vec (subs sp x s)
subs (IF e s1 s2) x s     = IF e (subs s1 x s) (subs s2 x s)

subsF :: Label -> S -> S -> S
subsF _ _ (RET e)          = RET e
subsF f s (ATRB x e sp)    = ATRB x e (subsF f s sp)
subsF f s (INV x g vec sp) | f == g = subs s x (subsF f s sp)
                           | otherwise = INV x g vec (subsF f s sp)
subsF f s (IF e s1 s2)     = IF e (subsF f s s1) (subsF f s s2) 

compile :: Program -> Program
compile p = compileAux ( getProgramRefList p ) p

compileAux :: [Label] -> Program -> Program
compileAux (l:ls) p = let 
        replaceIn (Just x) =
          map (\(PROC lp vec s) -> PROC lp vec (subsF l s (getProcBody x))) p
        replaceIn Nothing  = compileAux ls p
        in replaceIn (findProc p l)



-- Functional Compiler

subsL :: M -> Label -> M -> M
subsL (LRET b) x m            = BNDR x b m
subsL (BNDR y b mp) x m       = BNDR y b (subsL mp x m)
subsL (LINV y f vec mp) x m   = LINV y f vec (subsL mp x m)
subsL (LIF b m1 m2) x m       = LIF b (subsL m1 x m) (subsL m2 x m)

subsLF :: Label -> M -> M -> M
subsLF _ _ (LRET b)           = LRET b
subsLF f m (BNDR x b mp)      = BNDR x b (subsLF f m mp)
subsLF f m (LINV x g vec mp)  | f == g = subsL m x (subsLF f m mp)
                              | otherwise = LINV x g vec (subsLF f m mp)
subsLF f m (LIF b m1 m2)      = LIF b (subsLF f m m1) (subsLF f m m2) 

compileL:: LProgram -> LProgram
compileL p = compileLAux ( getLProgramRefList p ) p

compileLAux :: [Label] -> LProgram -> LProgram
compileLAux (l:ls) p = let 
        replaceIn (Just x) = 
         map (\(LPROC lp vec s) -> 
          LPROC lp vec (subsLF l s (getLProcBody x))) p
        replaceIn Nothing  = compileLAux ls p
        in replaceIn (findLProc p l)
