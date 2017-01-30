module MiniLogo where


import Prelude hiding (Num)

type Num = Int
type Var = String
type Macro = String

type Prog = [Cmd]

data Mode = Up | Down
            deriving (Eq,Show)

data Expr = Var Var
          | Num Num
          | Add Expr Expr
            deriving (Eq,Show)

data Cmd = Pen Mode
         | Move Expr Expr
         | Define Macro [Var] Prog
         | Call Macro [Expr]
           deriving (Eq,Show)