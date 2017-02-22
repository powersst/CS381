-- | A simple expression language with two types.
module IntBool where


--  Syntax of the IntBool language:
--
--  int  ::=  (any integer)
--  bool ::=  true  |  false
--
--  exp  ::=  int              integer literal
--        |   exp + exp        integer addition
--        |   exp = exp        check whether two values are equal
--        |   !exp             boolean negation


-- 1. Define the abstract syntax as a Haskell data type.

data Exp = Lit Int
         | Add Exp Exp
         | Equ Exp Exp
         | Not Exp
  deriving (Eq,Show)

-- Here are some example expressions:
--  * draw the abstract syntax trees (exercise)
--  * what should the result be?
-- x = Add (Lit 2) (Add (Lit 3) (Lit 4))  -- 9 :: Int
-- y = Not (Equ x (Lit 10))               -- True :: Bool
-- z = Not x                              -- type error!


-- 2. Identify/define the semantic domain for this language
--   * what types of values can we have?
--   * how can we express this in Haskell?

data Value = I Int
           | B Bool
           | TypeError
  deriving (Eq,Show)

-- data Maybe a    = Just a | Nothing
-- data Either a b = Left a | Right b

-- Alternative semantics domain using Maybe and Either:
--
--   type Value = Maybe (Either Int Bool)
--
-- Example semantic values in both representations:
--
--     I 3      <=>  Just (Left 3)
--   B True     <=>  Just (Right True)
--   TypeError  <=>  Nothing


-- 3. Define the semantic function
sem :: Exp -> Value
sem (Lit i)   = I i
sem (Add l r) = case (sem l, sem r) of
                  (I i, I j) -> I (i+j)
                  _          -> TypeError
sem (Equ l r) = case (sem l, sem r) of
                  (I i, I j) -> B (i == j)
                  (B a, B b) -> B (a == b)
                  _          -> TypeError
sem (Not e)   = case sem e of
                  B b -> B (not b)
                  _   -> TypeError


--
-- Statically typed variant
--

-- 1. Define the syntax of types
data Type = TInt | TBool | TypeErr


-- 2. Define the typing relation.
typeOf :: Exp -> Type
typeOf (Lit _)   = TInt
typeOf (Add l r) = case (typeOf l, typeOf r) of
                     (TInt, TInt) -> TInt
                     _            -> TypeErr
typeOf (Equ l r) = case (typeOf l, typeOf r) of
                     (TInt, TInt)   -> TBool
                     (TBool, TBool) -> TBool
                     _              -> TypeErr
typeOf (Not e)   = case typeOf e of
                     TBool -> TBool
                     _     -> TypeErr


-- 3. Define the semantics of type-correct programs.
sem' :: Exp -> Either Int Bool
sem' (Lit i)   = Left i
sem' (Add l r) = Left (evalInt l + evalInt r)
sem' (Equ l r) = Right (sem' l == sem' r)
sem' (Not e)   = Right (not (evalBool e))


-- | Helper function to evaluate an Exp to an Int
evalInt :: Exp -> Int
evalInt e = case sem' e of
              Left i -> i
              _ -> error "internal error: expected Int got something else!"

-- | Helper function to evaluate an Exp to an Bool
evalBool :: Exp -> Bool
evalBool e = case sem' e of
              Right b -> b
              _ -> error "internal error: expected Bool got something else!"


-- 4. Define our interpreter.
eval :: Exp -> Value
eval e = case typeOf e of
           TInt    -> I (evalInt e)
           TBool   -> B (evalBool e)
           TypeErr -> TypeError
