--Steven Powers, Christian Mello, and Erik Watterson

module MiniLogo where

import Prelude hiding(Num)
import Data.List

--Exercise 1
type Num = Int
type Var = String
type Macro = String

type Prog = [Cmd]

data Mode = Down | Up
    deriving (Eq, Show)

data Expr = Var Var
    | Num Num
    | Add Expr Expr
    deriving (Eq, Show)

data Cmd = Pen Mode
    | Move Expr Expr
    | Define Macro [Var] Prog
    | Call Macro [Expr]
    deriving (Eq, Show)

--Exercise 2 -----------------------
-- define (line (x1,y1,x2,y2))
-- Draw a line from (x1,y1) to (x2, y2)

line :: Cmd
line = Define "line"
              ["x1", "y1", "x2", "y2"]
              [Pen Up, Move (Var "x1") (Var "y1"),
              Pen Down, Move (Var "x2") (Var "y2")]

--Exercise 3 -----------------------
{-
Use the line macro you just defined to define a new MiniLogo macro
nix (x,y,w,h) that draws a big “X” of width w and height h,
starting from position (x,y).
Your definition should not contain any move commands.

Write the macro in MiniLogo concrete syntax and include this
  definition in a comment in your submission.
Encode the macro definition as a Haskell value, representing
  the abstract syntax of the definition.
-}

-- define nix(x, y, w, h){
--  line(x, y, x+w, y+h)
--  line(x + w, y, x, y+h)
--}

-- This defines nix as type command, and then defines nix as a macro
-- with a list of vars: x,y,w,h and Prog, which is a list of commands.
-- So then within the [list] of commands we have two call functions to the
-- line

nix :: Cmd
nix = Define "nix"
              ["x", "y", "w", "h"]
              [Call "line"
                  [Var "x", Var "y", Add (Var "x") (Var "w"),
                  Add (Var "y") (Var "h")],
              Call "line"
                  [Add (Var "x") (Var "w"), Var "x", Var "y",
                  Add (Var "y") (Var "h")]]

--Exercise 4


{-
Define a Haskell function steps :: Int -> Prog that constructs a
MiniLogo program that draws a staircase of n steps starting from (0,0).
Below is a visual illustration of what the generated program should
draw for a couple different applications of steps.
-}

--Exercise 5
{-
Define a Haskell function macros :: Prog -> [Macro] that returns a
list of the names of all of the macros that are defined anywhere
in a given MiniLogo program. Don’t worry about duplicates—if a macro
is defined more than once, the resulting list may include multiple
copies of its name.
-}


--Exercise 6

{-
Define a Haskell function pretty :: Prog -> String
that pretty-prints a MiniLogo program. That is, it
transforms the abstract syntax (a Haskell value)
into nicely formatted concrete syntax (a string of characters).
Your pretty-printed program should look similar to the example
programs given above; however, for simplicity you will probably
want to print just one command per line.

In GHCi, you can render a string with newlines by applying the
 function putStrLn. So, to pretty-print a program p use:
 putStrLn (pretty p).
-}


--Exercise 7
{-
Define a Haskell function optE :: Expr -> Expr that partially
evaluates expressions by replacing any additions of literals
with the result. For example, given the expression (2+3)+x,
optE should return the expression 5+x.
-}

--Exercise 8
{-
Define a Haskell function optP :: Prog -> Prog that
optimizes all of the expressions contained in a
given program using optE.
-}
