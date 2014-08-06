{-
 Module Languages

 Description: module that defines language structures
-}
module Languages where

type Label = String

-- Imperative expressions
data E = VAR Label | CNST Int
-- Functional expressions
data B = ID Label  | LCNST Int

-- Imperative comand sequences
data S  = ATRB Label E S             
	| INV Label Label [E] S 
	| IF E S S                  
	| RET E	             

-- Functional command sequences
data M  = BNDR Label B M              
	| LINV Label Label [E] M 
	| LIF B M M                  
	| LRET B                      

-- Imperative Procedure
data Proc  = PROC Label [Label] S 
-- Functional Procedure
data LProc = LPROC Label [Label] M

-- Imperative Program
type Program = [Proc] 
-- Functional Program
type LProgram = [LProc] 



instance Show E where
	show e = case e of
		VAR x   -> x
		CNST c  -> show c

instance Show S where
	show s = case s of
		RET e 	      -> list2string
						 ["ret(",show e,")"]
		ATRB x e s    -> list2string
						 [x,":=",show e,";\n"
						 ,show s]
		INV x f vec s -> list2string
						 [x,":=",f,"("
						 ,printVector show vec,");\n"
						 ,show s]
	 	IF e s1 s2	  -> list2string
	 		 			 ["if(",show e,"){\n"
						 ,show s1
	 					 ,"\n}else{\n"
	 					 ,show s2
	 					 ,"\n}"] 

instance Show Proc where
	show (PROC l vec s) = list2string
						  [l,":=proc("
						  ,printVector id vec,"){\n"
						  ,show s
						  ,"\n}\n"]


instance Show B where
	show b = case b of
		ID x     -> list2string ["\\k.k", x]
		LCNST c  -> list2string ["\\k.k", show c]

instance Show M where
	show m = case m of
		LRET b 	       -> list2string
						   ["(\\k.k(",show b,"))"]
		BNDR x b m     -> list2string 
						  ["(",show b
						  	,")(\\"
						  	,x,"."
						  	,show m,")"]
		LINV x f vec m -> list2string
						  ["(",f,"",printVector show vec," )("
						  	,show m
						  	,")"]
	 	LIF b m1 m2	   -> list2string
	 				      ["(if ",show b
	 				      	," (",show m1,")"
							," (",show m2,"))"] 

instance Show LProc where
	show (LPROC l vec m) = list2string
						   [l,":=\\proc "
						   , printVector id vec,")."
						   ,show m
						   ,"\n"]

printVector:: (Show a) => (a -> String) -> [a] -> String
printVector f e = case e of 
          []    -> ""
          e1:[] -> f e1
          _ -> (foldr (++) ((f.last) e) 
                    (map ((++",").f) 
                        ((reverse.tail.reverse) e)))
                        
list2string :: [String] -> String
list2string = foldr (++) ""
