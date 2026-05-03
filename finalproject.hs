{-# LANGUAGE GADTs,FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

import Control.Monad

--boilerplate necessities
{-
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
    | if T then T else T 
    | T && T 
    | T || T 
    | T <= T 
    | isZero T 
    | Fix T 

TY ::= Num 
     | Boolean 
     | TY -> TY
-}
--Internal (may need to move some of these around) --
data Cb where
    Num :: Int -> Cb  
    Bool :: Bool -> Cb
    Plus :: Cb -> Cb -> Cb 
    Minus :: Cb -> Cb -> Cb
    Mult :: Cb -> Cb -> Cb 
    Div :: Cb -> Cb -> Cb  
    Exp :: Cb -> Cb -> Cb
    Between :: Cb -> Cb -> Cb -> Cb 
    Lambda :: String -> CbTy -> Cb -> Cb 
    App :: Cb -> Cb -> Cb
    Id :: String -> Cb
    If :: Cb -> Cb -> Cb -> Cb
    And :: Cb -> Cb -> Cb
    Or :: Cb -> Cb -> Cb
    Leq :: Cb -> Cb -> Cb
    IsZero :: Cb -> Cb 
    Fix :: Cb -> Cb
    deriving (Show,Eq)
    
data CbTy where
  TNum :: CbTy
  TBool :: CbTy
  (:->:) :: CbTy -> CbTy -> CbTy
  TClosure :: String -> CbTy -> Cont -> CbTy
  deriving (Show,Eq)

--Data Types--
data CbVal where
    NumV :: Int -> CbVal
    BooleanV :: Bool -> CbVal
    ClosureV :: String -> CbTy -> Cb -> EnvVal -> CbVal
    deriving (Show,Eq)

--External--
data CbExt where
  NumX :: Int -> CbExt
  BooleanX :: Bool -> CbExt
  IdX :: String -> CbExt  
  PlusX :: CbExt -> CbExt -> CbExt
  MinusX :: CbExt -> CbExt -> CbExt
  MultX :: CbExt -> CbExt -> CbExt
  DivX :: CbExt -> CbExt -> CbExt
  ExpX :: CbExt -> CbExt -> CbExt
  BetweenX :: CbExt -> CbExt -> CbExt -> CbExt
  LambdaX :: String -> CbTy -> CbExt -> CbExt
  AppX :: CbExt -> CbExt -> CbExt 
  BindX :: String -> CbTy -> CbExt -> CbExt -> CbExt
  IfX :: CbExt -> CbExt -> CbExt -> CbExt
  AndX :: CbExt -> CbExt -> CbExt
  OrX :: CbExt -> CbExt -> CbExt
  LeqX :: CbExt -> CbExt -> CbExt
  IsZeroX :: CbExt -> CbExt
  FixX :: CbExt -> CbExt
  deriving (Show,Eq)

-- Environment Definitions
type EnvVal = [(String, CbVal)]
type Cont = [(String, CbTy)]

-- Reader & Helper Methods (boilerplate from p5)
data Reader e a = Reader (e -> Maybe a)

ask :: Reader a a 
ask = Reader $ \e -> Just e

runR :: Reader e a -> e -> Maybe a
runR (Reader f) e = f e 

local :: (e -> t) -> Reader t a -> Reader e a
local f r = Reader $ \e -> runR r (f e)

useClosure :: String -> CbVal -> EnvVal -> EnvVal -> EnvVal
useClosure i v e _ = (i,v):e

instance Monad (Reader e) where
    g >>= f = Reader $ \e -> 
        case runR g e of
      Nothing -> Nothing
      Just v  -> runR (f v) e

instance Functor (Reader e) where
    fmap f (Reader g) = Reader $ \e ->
        case g e of
        Nothing -> Nothing
        Just v  -> Just (f v)

instance Applicative (Reader e) where
    pure x = Reader $ \e -> Just x
    (Reader f) <*> (Reader g) = Reader $ \e ->
        case f e of
      Nothing -> Nothing
      Just h  ->
        case g e of
          Nothing -> Nothing
          Just x  -> Just (h x)

instance MonadFail (Reader e) where
  fail _ = Reader $ \_ -> Nothing

--more boilerplate to edit; make sure these are actually CORRECT--
typeof :: Cb -> Reader Cont CbTy
typeof (Num x) = return TNum
typeof (Bool x) = return TBool
typeof (Id id) = do { cont <- ask;
  case lookup id cont of
    Just t -> return t
    Nothing -> fail "unbound id"}
typeof (Plus l r) = do {l' <- (typeof l);
  r' <- (typeof r);
  (if (l'==TNum && r'==TNum) then return TNum else fail "type fail in plus")}  
typeof (Minus l r) = do {l' <- (typeof l);
  r' <- (typeof r);
  (if (l'==TNum && r'==TNum) then return TNum else fail "type fail in minus")}  
typeof (Mult l r) = do {l' <- (typeof l);
  r' <- (typeof r);
  (if (l'==TNum) && (r'==TNum) then return TNum else fail "type fail in mult")  } 
typeof (Div l r) = do {l' <- (typeof l);
  r' <- (typeof r);
  (if (l'==TNum && r'==TNum) then return TNum else fail "type fail in div")  } 
typeof (Exp l r) = do {l' <- (typeof l);
  r' <- (typeof r);
  (if (l'==TNum && r'==TNum) then return TNum else fail "type fail in exp")  }  
typeof (Between l m r) = do {l' <- (typeof l);
  r' <- (typeof r);
  m' <- (typeof m);
  (if (l'==TNum && r'==TNum && m'==TNum) then return TNum else fail "type fail in between")  }  
typeof (Lambda x t b) = do {
  b' <- local((x,t):) (typeof b);
  return (t :->: b')}  
typeof (App f a) = do {
  (fD :->: fR) <- typeof f;
  a' <- typeof a;
  if fD == a' then return fR else fail "type fail in app"} 
typeof (If c t e) = do {c' <- (typeof c);
  t' <- (typeof t);
  e' <- (typeof e);
  (if (c' == TBool && (t' == e')) then return t' else fail "type fail in if") }   
typeof (And l r) = do {l' <- (typeof l);
  r' <- (typeof r);
  (if (l'==TBool && r'==TBool) then return TBool else fail "type fail in and")  }  
typeof (Or l r) = do {l' <- (typeof l);
  r' <- (typeof r);
  (if (l'==TBool && r'==TBool) then return TBool else fail "type fail in or")  }
typeof (IsZero x) = do {x' <- (typeof x);
  (if (x' == TNum) then return TBool else fail "type fail in iszero")  }
typeof (Leq l r) = do {l' <- (typeof l);
  r' <- (typeof r);
  if (r' == TNum && l' == TNum) then return TBool else fail "type fail in leq"}
typeof (Fix f) = do {(d :->: r) <- (typeof f);
  return r}
  
elab :: CbExt -> Cb 
elab (NumX x) = (Num x)
elab (PlusX l r) = (Plus (elab l) (elab r))
elab (MinusX l r) = (Minus (elab l) (elab r))
elab (MultX l r) = (Mult (elab l) (elab r))
elab (DivX l r) = (Div (elab l) (elab r))
elab (ExpX b e) = (Exp (elab b) (elab e))
elab (LambdaX i t b) = (Lambda i t (elab b))
elab (AppX f a) = (App (elab f) (elab a)) 
elab (BindX i t v b) = App (Lambda i t (elab b))(elab v) 
elab (IdX id) = (Id id)
elab (LoopX cond var b) = (Fix (Lambda loop in (Lambda var in if (elab cond) then loop (var + 1) else var)))

eval :: Cb -> Reader EnvVal CbVal
eval (Num x) = return (NumV x)
eval (Bool x) = return (BooleanV x)
eval (Id id) = do { env <- ask;
  case (lookup id env) of
    Just x -> return x
    Nothing -> fail "unbound id"}
eval (Plus l r) = do {(NumV l') <- (eval l);
  (NumV r') <- (eval  r);
  return (NumV (l' + r'))}  
eval (Minus l r) = do {(NumV l') <- (eval l);
  (NumV r') <- (eval  r);
  return (NumV (l' - r'))}
eval (Mult l r) = do {(NumV l') <- (eval  l);
  (NumV r') <- (eval  r);
  return (NumV (l' * r'))} 
eval (Div l r) = do {(NumV l') <- (eval l); 
  (NumV r') <- (eval  r);
  if r' == 0 then fail "div by 0" else return (NumV (l' `div` r'))}  
eval (Exp b e) = do {(NumV b') <- (eval  b);
  (NumV e') <- (eval  e);
  if e' < 0 then fail "negative exponent" else return (NumV (b' ^ e'))}
eval (Between l m r) = do { (NumV l') <- (eval l);
  (NumV m') <- (eval m);
  (NumV r') <- (eval r);
  return (BooleanV ((m' > l') && (m' < r')))}
eval (Lambda i t b) = do { env <- ask;
  return (ClosureV i t b env)}
eval (App f a) = do {(ClosureV i t b e) <- (eval f);
  a' <- (eval a);
  local (useClosure i a' e)(eval b) } 
eval (If c t e) = do {(BooleanV c') <- (eval c);
  if c' then (eval  t) else (eval  e)}
eval (And l r) = do { (BooleanV l') <- (eval l);
  (BooleanV r') <- (eval r);
  return (BooleanV (l' && r'))}
eval (Or l r) = do { (BooleanV l') <- (eval l);
  (BooleanV r') <- (eval r);
  return (BooleanV (l' || r'))} --is this how or works in haskell? no idea
eval (IsZero x) = do { (NumV x') <- (eval x);
  return (BooleanV (x' == 0))}
eval (Leq l r) = do {(NumV l') <- (eval l);
  (NumV r') <- (eval r);
  return (BooleanV (l' <= r'))}
eval (Fix f) = do 
  (ClosureV i t b e) <- (eval f)
  let fixVal = ClosureV i t b ((i, fixVal):e)
  local (const ((i, fixVal):e))(eval b)
