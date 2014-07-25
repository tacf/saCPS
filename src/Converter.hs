{-
 Module Converter

 Description: module that define language convertions
-}
module Converter(la2cps, cps2la) where

import Languages

-- LA -> CPS
la2cps  :: Proc -> LProc
la2cps (PROC l vec s) = (LPROC l vec (la2cpsS s))

la2cpsS :: S -> M
la2cpsS s = case s of
 (RET e)         -> LRET (la2cpsE e)
 (ATRB l e s)    -> BNDR l (la2cpsE e) (la2cpsS s)
 (INV l f vec s) -> LINV l f vec (la2cpsS s)
 (IF e s1 s2)    -> LIF (la2cpsE e) (la2cpsS s1) (la2cpsS s2) 

la2cpsE :: E -> B
la2cpsE e = case e of
 (VAR l)  -> ID l
 (CNST x) -> LCNST x


-- CPS -> LA
cps2la  :: LProc -> Proc
cps2la (LPROC l vec m) = PROC l vec (cps2laM m)

cps2laM :: M -> S
cps2laM m = case m of
 (LRET b)         -> RET (cps2laB b)
 (BNDR l b m)     -> ATRB l (cps2laB b) (cps2laM m)
 (LINV l f vec m) -> INV l f vec (cps2laM m) 
 (LIF b m1 m2)    -> IF (cps2laB b) (cps2laM m1) (cps2laM m2)

cps2laB :: B -> E
cps2laB b = case b of
 (ID l)    -> VAR l
 (LCNST x) -> CNST x