module VM (
		-- Imperative Execution
        runProgram,
		-- Functional Execution
        runLProgram
		) where

import Data.List

import Languages


type Assoc = [(Label,Int)]

type State   = Assoc
type Closure = Assoc

type Config = (S,State)
type LConfig = (M,Closure)

evalInState :: E -> State -> Int
evalInState (CNST c) _ = c
evalInState (VAR l) s = case (find ((==l).fst)  s) of
						Nothing -> 0
						(Just x) -> snd x

evalInClosure :: B -> Closure -> Maybe Int
evalInClosure (LCNST c) _ = Just c
evalInClosure (ID l) c = fmap snd (find ((==l).fst) c)


updateState :: Label -> Int -> State -> State
updateState  = updateAssoc

updateClosure :: Label -> Int -> Closure -> Closure
updateClosure = updateAssoc 


updateAssoc :: Label -> Int -> Assoc -> Assoc
updateAssoc l c [] = [(l,c)]
updateAssoc l c ((x,v):xs) | x==l = (x,c):xs
						   | otherwise = (x,v):(updateAssoc l c xs)


isFinalConfig :: Config -> Bool
isFinalConfig (RET _, _) = True
isFinalConfig (_,_) = False 

isFinalLConfig :: LConfig -> Bool
isFinalLConfig (LRET _, _) = True
isFinalLConfig (_,_) = False

step :: Config -> Config
step (RET e, st) = (RET e, st)
step (ATRB x e s, st) = (s,updateState x (evalInState e st) st)
-- step (INV x l vec s, st) = no specific semantics
step (IF e s1 s2, st) = case (evalInState e st) of
						 0 -> (s2, st)
						 _ -> (s1, st)
                        

stepL :: LConfig -> LConfig
stepL (LRET b, cl) = (LRET b, cl)
stepL (BNDR x b m, cl) = case evalInClosure b cl of
						 (Just b1) -> (m,updateClosure x b1 cl)
						 -- Special Config representing error in closure
						 Nothing -> (LRET (ID "error"),[])
-- stepL (LINV x l vec m, cl) = no specific semantics
stepL (LIF b m1 m2, cl) = case (evalInClosure b cl) of
						(Just 0) -> (m2, cl)
						(Just x) -> (m1, cl)
						Nothing -> (LRET (ID "error"), [])

runProgram :: Config -> IO ()
runProgram c = do
                let x = step c
                putStrLn ("###Command###\n"
                         ++show (fst x)
                         ++"\n####State####\n"
                         ++show (snd x)
                         ++"\n#############\n"
                         ++"---------------------")
                if (isFinalConfig x)
                then putStrLn "Computation Ended"
                else runProgram x

runLProgram :: LConfig -> IO ()
runLProgram c = do
                let x = stepL c
                putStrLn ("###Command###\n"
                         ++show (fst x)
                         ++"\n####Closure####\n"
                         ++show (snd x)
                         ++"\n#############\n"
                         ++"---------------------")
                if (isFinalLConfig x)
                then putStrLn "Computation Ended"
                else runLProgram x