module IntBool where

data Exp = Lit Int
         | Add Exp Exp
         | Equ Exp Exp
         | Not Exp
  deriving (Eq,Show)

x = Add (Lit 2) (Add (Lit 3) (Lit 4))  -- 9 :: Int
y = Not (Equ x (Lit 10))               -- True :: Bool
z = Not x                              -- type error!

data Value = I Int
           | B Bool
           | TypeError
  deriving (Eq,Show)

sem :: Exp -> Value
