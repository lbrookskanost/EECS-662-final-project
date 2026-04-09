{-
boilerplate necessities
T ::= int 
    | bool 
    | id 
    | T + T 
    | T - T 
    | T * T 
    | T / T 
    | T ^ T 
    | between T T T 
    | lambda (id:TY) in T 
    | T T  --what is this??
    | bind id T T 
    | if T then T else T 
    | T && T 
    | T || T 
    | T <= T 
    | isZero T 
    | Fix T --?

TY ::= Num 
     | Boolean 
     | TY -> TY
-}

-- Abstract Syntax Definitions --

--Internal Syntax (may need to move some of these around) --
data KULang where
    Num :: Int -> KULang  
    Plus :: KULang -> KULang -> KULang 
    Minus :: KULang -> KULang -> KULang
    Mult :: KULang -> KULang -> KULang 
    Div :: KULang -> KULang -> KULang  
    Exp :: KULang -> KULang -> KULang
    Between :: KULang -> KULang -> KULang -> KULang 
    Lambda :: String -> KULang -> KULang 
    If :: KULang -> KULang -> KULang -> KULang
    And :: KULang -> KULang -> KULang
    Or :: KULang -> KULang -> KULang
    Leq :: KULang -> KULang -> KULang
    isZero :: KULang -> KULang 
    Id :: String -> KULang -- ?
    App :: KULang -> KULang -> KULang -- ?
    deriving (Show,Eq)
