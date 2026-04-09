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
    | T T 
    | bind id T T 
    | if T then T else T 
    | T && T 
    | T || T 
    | T <= T 
    | isZero T 
    | Fix T

TY ::= Num 
     | Boolean 
     | TY -> TY
