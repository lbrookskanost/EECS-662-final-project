{-# LANGUAGE GADTs,FlexibleContexts #-}

-- paste ur finalproject_fixed.hs content here, or load it as a module.
-- these tests assume `interpret` is in scope.

import Control.Monad.State
import Control.Monad.Reader

-- ============================================================
-- HELPERS
-- ============================================================

-- run a test and print pass/fail
runTest :: (Show a, Eq a) => String -> Maybe a -> Maybe a -> IO ()
runTest name expected actual =
  if expected == actual
    then putStrLn $ "[PASS] " ++ name
    else putStrLn $ "[FAIL] " ++ name
               ++ "\n  expected: " ++ show expected
               ++ "\n  actual:   " ++ show actual

-- ============================================================
-- LITERALS
-- ============================================================

test_num :: IO ()
test_num = runTest "Num literal"
  (Just (NumV 42))
  (interpret (NumX 42))

test_bool_true :: IO ()
test_bool_true = runTest "Bool literal true"
  (Just (BooleanV True))
  (interpret (BooleanX True))

test_bool_false :: IO ()
test_bool_false = runTest "Bool literal false"
  (Just (BooleanV False))
  (interpret (BooleanX False))

-- ============================================================
-- ARITHMETIC
-- ============================================================

test_plus :: IO ()
test_plus = runTest "Plus 3 4"
  (Just (NumV 7))
  (interpret (PlusX (NumX 3) (NumX 4)))

test_minus :: IO ()
test_minus = runTest "Minus 10 3"
  (Just (NumV 7))
  (interpret (MinusX (NumX 10) (NumX 3)))

test_mult :: IO ()
test_mult = runTest "Mult 6 7"
  (Just (NumV 42))
  (interpret (MultX (NumX 6) (NumX 7)))

test_div :: IO ()
test_div = runTest "Div 20 4"
  (Just (NumV 5))
  (interpret (DivX (NumX 20) (NumX 4)))

test_div_by_zero :: IO ()
test_div_by_zero = runTest "Div by zero => Nothing"
  Nothing
  (interpret (DivX (NumX 5) (NumX 0)))

test_exp :: IO ()
test_exp = runTest "Exp 2 10"
  (Just (NumV 1024))
  (interpret (ExpX (NumX 2) (NumX 10)))

test_exp_neg :: IO ()
test_exp_neg = runTest "Exp negative => Nothing"
  Nothing
  (interpret (ExpX (NumX 2) (NumX (-1))))

-- ============================================================
-- BETWEEN  (should produce BooleanV -- this was the type bug)
-- ============================================================

test_between_true :: IO ()
test_between_true = runTest "Between 1 5 10 => true (5 strictly between 1 and 10)"
  (Just (BooleanV True))
  (interpret (BetweenX (NumX 1) (NumX 5) (NumX 10)))

test_between_false_edge :: IO ()
test_between_false_edge = runTest "Between 1 1 10 => false (edge, not strictly greater)"
  (Just (BooleanV False))
  (interpret (BetweenX (NumX 1) (NumX 1) (NumX 10)))

test_between_false_outside :: IO ()
test_between_false_outside = runTest "Between 1 11 10 => false (outside range)"
  (Just (BooleanV False))
  (interpret (BetweenX (NumX 1) (NumX 11) (NumX 10)))

-- ============================================================
-- BOOLEAN OPS
-- ============================================================

test_and_tt :: IO ()
test_and_tt = runTest "And true true"
  (Just (BooleanV True))
  (interpret (AndX (BooleanX True) (BooleanX True)))

test_and_tf :: IO ()
test_and_tf = runTest "And true false"
  (Just (BooleanV False))
  (interpret (AndX (BooleanX True) (BooleanX False)))

test_or_ff :: IO ()
test_or_ff = runTest "Or false false"
  (Just (BooleanV False))
  (interpret (OrX (BooleanX False) (BooleanX False)))

test_or_ft :: IO ()
test_or_ft = runTest "Or false true"
  (Just (BooleanV True))
  (interpret (OrX (BooleanX False) (BooleanX True)))

test_iszero_true :: IO ()
test_iszero_true = runTest "IsZero 0 => true"
  (Just (BooleanV True))
  (interpret (IsZeroX (NumX 0)))

test_iszero_false :: IO ()
test_iszero_false = runTest "IsZero 5 => false"
  (Just (BooleanV False))
  (interpret (IsZeroX (NumX 5)))

test_leq_true :: IO ()
test_leq_true = runTest "Leq 3 5 => true"
  (Just (BooleanV True))
  (interpret (LeqX (NumX 3) (NumX 5)))

test_leq_equal :: IO ()
test_leq_equal = runTest "Leq 5 5 => true (equal)"
  (Just (BooleanV True))
  (interpret (LeqX (NumX 5) (NumX 5)))

test_leq_false :: IO ()
test_leq_false = runTest "Leq 6 5 => false"
  (Just (BooleanV False))
  (interpret (LeqX (NumX 6) (NumX 5)))

-- ============================================================
-- IF
-- ============================================================

test_if_true :: IO ()
test_if_true = runTest "If true 1 2 => 1"
  (Just (NumV 1))
  (interpret (IfX (BooleanX True) (NumX 1) (NumX 2)))

test_if_false :: IO ()
test_if_false = runTest "If false 1 2 => 2"
  (Just (NumV 2))
  (interpret (IfX (BooleanX False) (NumX 1) (NumX 2)))

-- type mismatch in branches => Nothing
test_if_type_mismatch :: IO ()
test_if_type_mismatch = runTest "If branches type mismatch => Nothing"
  Nothing
  (interpret (IfX (BooleanX True) (NumX 1) (BooleanX False)))

-- ============================================================
-- LAMBDA + APP + BIND
-- ============================================================

-- (\x:TNum. x + 1) 41 => 42
test_lambda_app :: IO ()
test_lambda_app = runTest "Lambda app: (\\x. x+1) 41 => 42"
  (Just (NumV 42))
  (interpret (AppX (LambdaX "x" TNum (PlusX (IdX "x") (NumX 1))) (NumX 41)))

-- bind sugar: let x:TNum = 10 in x * x => 100
test_bind :: IO ()
test_bind = runTest "Bind: let x=10 in x*x => 100"
  (Just (NumV 100))
  (interpret (BindX "x" TNum (NumX 10) (MultX (IdX "x") (IdX "x"))))

-- unbound id => Nothing
test_unbound :: IO ()
test_unbound = runTest "Unbound id => Nothing"
  Nothing
  (interpret (IdX "nope"))

-- ============================================================
-- FIX (recursion)
-- ============================================================

-- factorial 5 = 120
-- Fix (\fact: (TNum :->: TNum). \n:TNum. if n==0 then 1 else n * fact (n-1))
test_fix_factorial :: IO ()
test_fix_factorial = runTest "Fix: factorial 5 => 120"
  (Just (NumV 120))
  (interpret factX)
  where
    factX = AppX fixFact (NumX 5)
    fixFact = FixX (LambdaX "fact" (TNum :->: TNum)
                     (LambdaX "n" TNum
                       (IfX (IsZeroX (IdX "n"))
                            (NumX 1)
                            (MultX (IdX "n")
                                   (AppX (IdX "fact")
                                         (MinusX (IdX "n") (NumX 1)))))))

-- ============================================================
-- ARRAYS
-- ============================================================

-- [1,2,3] idx 1 => 2
test_arridx :: IO ()
test_arridx = runTest "ArrIdx [1,2,3] 1 => 2"
  (Just (NumV 2))
  (interpret (ArrIdxX (ArrX [NumX 1, NumX 2, NumX 3]) (NumX 1)))

-- out of bounds => Nothing
test_arridx_oob :: IO ()
test_arridx_oob = runTest "ArrIdx out of bounds => Nothing"
  Nothing
  (interpret (ArrIdxX (ArrX [NumX 1, NumX 2]) (NumX 5)))

-- negative index => Nothing
test_arridx_neg :: IO ()
test_arridx_neg = runTest "ArrIdx negative index => Nothing"
  Nothing
  (interpret (ArrIdxX (ArrX [NumX 1, NumX 2]) (NumX (-1))))

-- ArrSet: set index 0 of [1,2,3] to 99, then read it back => 99
test_arrset :: IO ()
test_arrset = runTest "ArrSet then ArrIdx => updated value 99"
  (Just (NumV 99))
  (interpret setAndRead)
  where
    -- let arr = [1,2,3]; ArrSet arr 0 99; ArrIdx arr 0
    setAndRead =
      BindX "arr" (TArr TNum) (ArrX [NumX 1, NumX 2, NumX 3])
        (BindX "_" TUnit (ArrSetX (IdX "arr") (NumX 0) (NumX 99))
          (ArrIdxX (IdX "arr") (NumX 0)))

-- ============================================================
-- WHILE  (the main bug: typeof While must return TUnit)
-- ============================================================

-- While false body => UnitV (never enters loop)
test_while_false_cond :: IO ()
test_while_false_cond = runTest "While false => UnitV (no iterations)"
  (Just UnitV)
  (interpret (WhileX (BooleanX False) (NumX 99)))

-- While with array mutation:
-- arr = [0]; while arr[0] < 3: arr[0] += 1  => arr[0] == 3
-- (demonstrates while actually running + array state persisting)
test_while_array_mutation :: IO ()
test_while_array_mutation = runTest "While: increment arr[0] until >= 3, then read => 3"
  (Just (NumV 3))
  (interpret whileLoop)
  where
    -- let arr = [0] in
    --   while (arr[0] <= 2):
    --     arr[0] := arr[0] + 1
    --   arr[0]
    whileLoop =
      BindX "arr" (TArr TNum) (ArrX [NumX 0])
        (BindX "_" TUnit
          (WhileX
            (LeqX (ArrIdxX (IdX "arr") (NumX 0)) (NumX 2))
            (ArrSetX (IdX "arr") (NumX 0)
              (PlusX (ArrIdxX (IdX "arr") (NumX 0)) (NumX 1))))
          (ArrIdxX (IdX "arr") (NumX 0)))

-- While cond not Bool => Nothing (type error)
test_while_type_err :: IO ()
test_while_type_err = runTest "While non-bool cond => Nothing (type error)"
  Nothing
  (interpret (WhileX (NumX 1) (NumX 99)))

-- ============================================================
-- TYPE ERRORS (misc)
-- ============================================================

test_plus_type_err :: IO ()
test_plus_type_err = runTest "Plus Bool Num => Nothing"
  Nothing
  (interpret (PlusX (BooleanX True) (NumX 1)))

test_app_type_err :: IO ()
test_app_type_err = runTest "App non-function => Nothing"
  Nothing
  (interpret (AppX (NumX 5) (NumX 1)))

--Internal (may need to move some of these around) --
data Cb where
    Num :: Int -> Cb  
    Bool :: Bool -> Cb
    Arr :: [Cb] -> Cb --not sure ab this
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
    While :: Cb -> Cb -> Cb
    ArrSet :: Cb -> Cb -> Cb -> Cb
    ArrIdx :: Cb -> Cb -> Cb
    deriving (Show,Eq)
    
data CbTy where
  TNum :: CbTy
  TBool :: CbTy
  TUnit :: CbTy
  (:->:) :: CbTy -> CbTy -> CbTy
  TArr :: CbTy -> CbTy
  deriving (Show,Eq)

--Data Types--
data CbVal where
    NumV :: Int -> CbVal
    BooleanV :: Bool -> CbVal
    ArrV :: Int -> Int -> CbVal --think so
    ClosureV :: String -> CbTy -> Cb -> EnvVal -> CbVal
    UnitV :: CbVal --dummy val
    deriving (Show,Eq)

--External--
data CbExt where
  NumX :: Int -> CbExt
  BooleanX :: Bool -> CbExt
  ArrX :: [CbExt] -> CbExt
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
  WhileX :: CbExt -> CbExt -> CbExt
  ArrSetX :: CbExt -> CbExt -> CbExt -> CbExt 
  ArrIdxX :: CbExt -> CbExt -> CbExt
  deriving (Show,Eq)

-- Environment Definitions
type EnvVal = [(String, CbVal)]
type Cont = [(String, CbTy)]
type Store = [(Int, CbVal)] --maybe ...????
type Eval a = ReaderT EnvVal (StateT Store Maybe) a

-- Reader & Helper Methods (boilerplate from p5)
useClosure :: String -> CbVal -> EnvVal -> EnvVal -> EnvVal
useClosure i v e _ = (i,v):e

alloc :: CbVal -> Store -> (Int, Store)
alloc val store =
  let addr = length store in (addr, store ++ [(addr, val)])
  
allocArray :: [CbVal] -> Store -> (Int, Store)
allocArray vals store =
  let startAddr = length store
      new = zip [startAddr..] vals
  in (startAddr, store ++ new)

deref :: Int -> Store -> Maybe CbVal
deref addr store = lookup addr store

set :: Int -> CbVal -> Store -> Maybe Store
set addr val store =
  case lookup addr store of
    Nothing -> Nothing
    Just _ -> Just (map (\(a,v) -> if a == addr then (a, val) else (a,v)) store)
    
--more boilerplate to edit; make sure these are actually CORRECT--
typeof :: Cb -> ReaderT Cont Maybe CbTy
typeof (Num x) = return TNum
typeof (Bool x) = return TBool
typeof (Arr []) = fail "empty array"
typeof (Arr xs) = do
  (TNum) <- typeof (head xs)
  ts <- mapM typeof (tail xs)
  if all (== TNum) ts
    then return (TArr TNum)
    else fail "arr elems must be integers"
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
  (if (l'==TNum && r'==TNum && m'==TNum) then return TBool else fail "type fail in between")  }  
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
  if d == r
    then return r
    else fail "type fail in fix"}
typeof (While cond body) = do 
  c' <- typeof cond
  b' <- typeof body
  if c' == TBool
    then return TUnit
    else fail "type fail in While"
typeof (ArrSet xs idx num) = do
  (TArr t) <- typeof xs
  idx' <- typeof idx
  num' <- typeof num
  if idx' == TNum && num' == t
    then return TUnit
    else fail "type fail in arrset"
typeof (ArrIdx xs idx) = do
  (TArr t) <- typeof xs
  idx' <- typeof idx
  if idx' == TNum
    then return t
    else fail "idx must be integer"
  
  
elab :: CbExt -> Cb 
elab (NumX x) = (Num x)
elab (BooleanX x) = Bool x
elab (ArrX xs) = (Arr (map elab xs))
elab (PlusX l r) = (Plus (elab l) (elab r))
elab (MinusX l r) = (Minus (elab l) (elab r))
elab (MultX l r) = (Mult (elab l) (elab r))
elab (DivX l r) = (Div (elab l) (elab r))
elab (ExpX b e) = (Exp (elab b) (elab e))
elab (LambdaX i t b) = (Lambda i t (elab b))
elab (AppX f a) = (App (elab f) (elab a)) 
elab (BindX i t v b) = App (Lambda i t (elab b))(elab v) 
elab (FixX f) = Fix (elab f)
elab (BetweenX l m r) = Between (elab l) (elab m) (elab r)
elab (AndX l r) = And (elab l) (elab r) 
elab (OrX l r) = Or (elab l) (elab r)   
elab (LeqX l r) = Leq (elab l) (elab r) 
elab (IsZeroX x) = IsZero (elab x)       
elab (IdX id) = (Id id)
elab (IfX c t e) = If (elab c) (elab t) (elab e)
elab (WhileX cond body) = (While (elab cond) (elab body))
elab (ArrSetX xs idx num) = (ArrSet (elab xs) (elab idx) (elab num))
elab (ArrIdxX xs idx) = (ArrIdx (elab xs) (elab idx))

eval :: Cb -> ReaderT EnvVal (StateT Store Maybe) CbVal
eval (Num x) = return (NumV x)
eval (Bool x) = return (BooleanV x)
eval (Arr xs) = do
  nums <- mapM eval xs
  store <- lift get
  let (addr, store') = allocArray nums store
  lift (put store')
  return (ArrV addr (length nums))
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
  return (BooleanV (l' || r'))}
eval (IsZero x) = do { (NumV x') <- (eval x);
  return (BooleanV (x' == 0))}
eval (Leq l r) = do {(NumV l') <- (eval l);
  (NumV r') <- (eval r);
  return (BooleanV (l' <= r'))}
eval (Fix f) = do 
  (ClosureV i t b e) <- (eval f)
  case b of
    Lambda n ty bd -> do
      let recenv = (i, fixVal) : e
          fixVal = ClosureV n ty bd recenv
      return fixVal
    _ -> fail "fix fail"
eval (While cond body) = do 
  (BooleanV cond') <- eval cond
  if cond' 
    then do 
      (eval body)
      eval (While cond body) 
    else return UnitV
eval (ArrSet xs idx num) = do
  (ArrV addr len) <- eval xs
  (NumV idx') <- eval idx
  num' <- eval num
  if idx' >= len || idx' < 0 then fail "indexerror" else do
    store <- lift get
    case set (addr + idx') num' store of
      Just store' -> do
        lift (put store')
        return UnitV
      Nothing -> fail "bad addr"
eval (ArrIdx xs idx) = do
  (ArrV addr len) <- eval xs
  (NumV idx') <- eval idx
  if idx' >= len || idx' < 0 then fail "indexerror" else do
    store <- lift get
    case deref (addr + idx') store of 
      Just num -> return num
      Nothing -> fail "indexerror"

runEval :: Eval a -> EnvVal -> Store -> Maybe (a, Store)
runEval e env store = runStateT (runReaderT e env) store

interpret :: CbExt -> Maybe CbVal
interpret t = 
  let e = elab t
      ty = runReaderT (typeof e) []
  in case ty of
    Nothing -> Nothing
    Just _ -> fmap fst (runEval (eval e) [] [])


-- ============================================================
-- MAIN
-- ============================================================

main :: IO ()
main = do
  putStrLn "=== LITERALS ==="
  test_num; test_bool_true; test_bool_false

  putStrLn "\n=== ARITHMETIC ==="
  test_plus; test_minus; test_mult
  test_div; test_div_by_zero
  test_exp; test_exp_neg

  putStrLn "\n=== BETWEEN (was returning TNum, now TBool) ==="
  test_between_true; test_between_false_edge; test_between_false_outside

  putStrLn "\n=== BOOLEAN OPS ==="
  test_and_tt; test_and_tf; test_or_ff; test_or_ft
  test_iszero_true; test_iszero_false
  test_leq_true; test_leq_equal; test_leq_false

  putStrLn "\n=== IF ==="
  test_if_true; test_if_false; test_if_type_mismatch

  putStrLn "\n=== LAMBDA / APP / BIND ==="
  test_lambda_app; test_bind; test_unbound

  putStrLn "\n=== FIX (recursion) ==="
  test_fix_factorial

  putStrLn "\n=== ARRAYS ==="
  test_arridx; test_arridx_oob; test_arridx_neg; test_arrset

  putStrLn "\n=== WHILE (was returning typeof body, now TUnit) ==="
  test_while_false_cond; test_while_array_mutation; test_while_type_err

  putStrLn "\n=== MISC TYPE ERRORS ==="
  test_plus_type_err; test_app_type_err
