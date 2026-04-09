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

-- Abstract Syntax Definitions; ripped from p4 and modified --

--Internal (may need to move some of these around) --
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
    IsZero :: KULang -> KULang 
    Id :: String -> KULang -- ?
    App :: KULang -> KULang -> KULang -- ?
    deriving (Show,Eq)

--Data Types--
data KULangVal where
    NumV :: Int -> KULangVal
    BoolV :: Bool -> KULangVal
    ClosureV :: String -> KULang -> EnvVal -> KULangVal
    deriving (Show,Eq)

--External--
data KULangExt where
    NumX :: Int -> KULangExt
    PlusX :: KULangExt -> KULangExt -> KULangExt
    MinusX :: KULangExt -> KULangExt -> KULangExt
    MultX :: KULangExt -> KULangExt -> KULangExt
    DivX :: KULangExt -> KULangExt -> KULangExt
    ExpX :: KULangExt -> KULangExt -> KULangExt
    If0X :: KULangExt -> KULangExt -> KULangExt -> KULangExt
    LambdaX :: String -> KULangExt -> KULangExt
    AppX :: KULangExt -> KULangExt -> KULangExt 
    BindX :: String -> KULangExt -> KULangExt -> KULangExt
    IdX :: String -> KULangExt
    deriving (Show,Eq)

-- Environment Definitions
type Env = [(String,KULang)]
type EnvVal = [(String,KULangVal)]

-- Reader Definition
data Reader e a = Reader (e -> Maybe a)

-- Monad Definition
instance Monad (Reader e) where
    g >>= f = Reader $ \e -> 
        case runR g e of
            Nothing -> Nothing
            Just x -> runR (f x) e

 -- Applicative Definition
instance Applicative (Reader e) where
pure x = Reader $ \e -> Just x
(Reader f) <*> (Reader g) = Reader $ \e -> case f e of
  Nothing -> Nothing
  Just fx -> case g e of
    Nothing -> Nothing
    Just gx -> Just (fx gx)

-- Functor Definition
instance Functor (Reader e) where
 fmap f (Reader g) = Reader $ \e -> case g e of
  Nothing -> Nothing
  Just x -> Just (f x)

-- Fail Definition
instance MonadFail (Reader e) where
fail _ = Reader $ \_ -> Nothing

-- Helper Methods
runR :: Reader e a -> e -> Maybe a
runR (Reader f) e = f e 

ask :: Reader a a 
ask = Reader $ \e -> Just e

local :: (e->t) -> Reader t a -> Reader e a
local f r = Reader $ \e -> (runR r (f e))

useClosure :: String -> KULangVal -> EnvVal -> EnvVal -> EnvVal
useClosure i v e _ = (i,v):e 
