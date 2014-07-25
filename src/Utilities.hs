{-
 Module Utilities

 Description: module  the define tools and predicates over
	      the languages
-}
module Utilities (
		  -- LA Functions
		  isSAProc, 
		  isWF, 
		  getProcBody, 
		  getProgramRefList, 
		  getProcRef, 
		  findProc,
		  -- CPS Functions
		  isLWF,
		  getLProcBody,
		  getLProgramRefList,
		  getLProcRef,
		  findLProc) where

import Languages
import Data.List

-- LA Language Functions


-- Indicates whether a LA procedure is in Single Assignment or not
isSAProc    :: Proc -> Bool
isSAProc p = snd (isSAaux [] (getProcBody p))

-- Auxiliar function for isSAproc
isSAaux :: [Label] -> S -> ([Label],Bool)
isSAaux l (RET _) = (l,True)
isSAaux l (ATRB x _ s) | (elem x l) = (l,False)
		               | otherwise  = isSAaux (x:l) s
isSAaux l (INV x _ _ s)| (elem x l) = (l,False)
		               | otherwise  = isSAaux (x:l) s
isSAaux l (IF _ s1 s2) = 
			 let (l1, b1) = isSAaux l s1
			     (l2, b2) = isSAaux l1 s2
			 in  (l2, b1 && b2 ) 

-- Indicates whether a LA program is well formed or not
isWF :: Program -> Bool
isWF pr = and (map (\p -> isWFProc [(getProcRef p)] pr (getProcBody p)) pr)

-- Indicates whether a LA procedure is well formed or not
isWFProc:: [Label] -> Program -> S -> Bool
isWFProc _ _ (RET _) 	     = True
isWFProc l p (ATRB _ _ s)    = isWFProc l p s
isWFProc l p (INV x f vec s)  
	| (elem f l) = False  -- Direct/Indirect recursion found
	| otherwise = 
		case (findProc p f) of
			Nothing -> False --Inexistent Procedure invoked
			(Just pr) -> 
				let (r,b) = (getProcRef pr, getProcBody pr)
				in	(isWFProc l p s) && (isWFProc (r:l) p b)
isWFProc l p (IF _ s1 s2)    = (isWFProc l p s1) && (isWFProc l p s2) 

-- Indicates whether a CPS program is well formed or not
isLWF :: LProgram -> Bool
isLWF pr = and (map (\p -> isLWFProc [(getLProcRef p)] pr (getLProcBody p)) pr)

-- Indicates whether a CPS procedure is well formed or not
isLWFProc:: [Label] -> LProgram -> M -> Bool
isLWFProc _ _ (LRET _)          = True
isLWFProc l p (BNDR _ _ m)      = isLWFProc l p m
isLWFProc l p (LINV x f vec m) 	
	| (elem f l) = False	-- Direct or Indirect recursion found
    | otherwise =
        case (findLProc p f) of
			Nothing -> False --Inexistent Procedure invoked
			(Just pr) -> 
				let (r,b) = (getLProcRef pr, getLProcBody pr)
				in	(isLWFProc l p m) && (isLWFProc (r:l) p b)
isLWFProc l p (LIF _ m1 m2)    = (isLWFProc l p m1) && (isLWFProc l p m2) 

-- Finds if a LA procedure with a specific reference exists
findProc :: Program -> Label -> Maybe Proc
findProc [] lf = Nothing
findProc ((PROC l vec s):ps) lf | lf == l = Just (PROC l vec s)
			    	| otherwise = findProc ps lf 

-- Finds if a CPS procedure with a specific reference exists
findLProc :: LProgram -> Label -> Maybe LProc
findLProc [] lf = Nothing
findLProc ((LPROC l vec m):ps) lf | lf == l = Just (LPROC l vec m)
			    | otherwise = findLProc ps lf 


-- Returns a list with all the references from a program
getProgramRefList :: Program -> [Label]
getProgramRefList = map (\(PROC l _ _) -> l) 

-- Returns a list with all the references from a program
getLProgramRefList :: LProgram -> [Label]
getLProgramRefList = map (\(LPROC l _ _) -> l) 

-- Returns LA procedure body
getProcBody :: Proc -> S
getProcBody (PROC _ _ s) = s

-- Returns CPS procedure body
getLProcBody :: LProc -> M
getLProcBody (LPROC _ _ m) = m

-- Returns LA procedure reference
getProcRef :: Proc -> Label
getProcRef (PROC l _ _) = l

-- Returns CPS procedure reference
getLProcRef :: LProc -> Label
getLProcRef (LPROC l _ _) = l