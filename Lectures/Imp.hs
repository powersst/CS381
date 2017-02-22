-- | A small imperative programming language.
module Imp where

import Data.Map (Map,fromList,lookup,insert)
import Data.Maybe (fromJust)
import Prelude hiding (lookup)


--
-- * Abstract syntax
--


-- | Variables.
type Var = String

-- | Abstract syntax of expressions.
--
--     expr  ::=  int
--            |   expr + expr
--            |   expr ≤ expr
--            |   `not` expr
--            |   var
--
data Expr = Lit Int        -- literal integer
          | Add Expr Expr  -- integer addition
          | LTE Expr Expr  -- less than or equal to
          | Not Expr       -- boolean negation
          | Ref Var        -- variable reference
  deriving (Eq,Show)


-- | Abstract syntax of statements.
--
--     stmt  ::=  var := expr
--            |   `if` expr stmt `else` stmt
--            |   `while` expr stmt
--            |   { stmt* }
--
data Stmt = Bind Var Expr
          | If Expr Stmt Stmt
          | While Expr Stmt
          | Block [Stmt]
  deriving (Eq,Show)


-- | Abstract syntax of types.
--     
--     type  ::=  `int`  |  `bool`
--
data Type = TInt | TBool
  deriving (Eq,Show)


-- | Abstract syntax of declarations.
--
--     decl  ::=  var : type
--
type Decl = (Var,Type)


-- | Abstract syntax of programs.
--
--     prog  ::=  decl* `begin` stmt
--
data Prog = P [Decl] Stmt
  deriving (Eq,Show)


-- | Example program: sum all of the numbers from 1 to 100.
--
--     sum : int
--     n : int
--     begin {
--       sum := 0
--       n := 1
--       while n <= 100 {
--         sum := sum + n
--         n := n + 1
--       }
--     }
ex1 :: Prog
ex1 = P [("sum",TInt),("n",TInt)]
        (Block [
          Bind "sum" (Lit 0),
          Bind "n" (Lit 1),
          While (LTE (Ref "n") (Lit 100))
            (Block [
              Bind "sum" (Add (Ref "sum") (Ref "n")),
              Bind "n" (Add (Ref "n") (Lit 1))
            ])
         ])

ex2 :: Prog
ex2 = P [("x",TInt)] (Bind "x" (LTE (Lit 3) (Lit 4)))


--
-- * Type system
--


-- | Variable environments.
type Env a = Map Var a


-- | Typing relation for expressions.
typeExpr :: Expr -> Env Type -> Maybe Type
typeExpr (Lit _)   _ = Just TInt
typeExpr (Add l r) m = case (typeExpr l m, typeExpr r m) of
                         (Just TInt, Just TInt) -> Just TInt
                         _                      -> Nothing
typeExpr (LTE l r) m = case (typeExpr l m, typeExpr r m) of
                         (Just TInt, Just TInt) -> Just TBool
                         _                      -> Nothing
typeExpr (Not e)   m = case typeExpr e m of
                         Just TBool -> Just TBool
                         _          -> Nothing
typeExpr (Ref v)   m = lookup v m


-- | Type checking statements.
typeStmt :: Stmt -> Env Type -> Bool
typeStmt (Bind v e)   m = case (lookup v m, typeExpr e m) of
                            (Just t, Just u) -> t == u
                            _                -> False
typeStmt (If c st se) m = (typeExpr c m == Just TBool)
                            && typeStmt st m
                            && typeStmt se m
typeStmt (While c sb) m = (typeExpr c m == Just TBool)
                            && typeStmt sb m
typeStmt (Block ss)   m = all (\s -> typeStmt s m) ss


-- | Type checking programs.
typeProg :: Prog -> Bool
typeProg (P ds s) = typeStmt s (fromList ds)


--
-- * Semantics
--

type Val = Either Int Bool

-- | Semantics of expressions.
evalExpr :: Expr -> Env Val -> Val
evalExpr (Lit i)   _ = Left i
evalExpr (Add l r) m = Left (evalInt l m + evalInt r m)
evalExpr (LTE l r) m = Right (evalInt l m <= evalInt r m)
evalExpr (Not e)   m = Right (not (evalBool e m))
evalExpr (Ref x)   m = case lookup x m of
                         Just v  -> v
                         Nothing -> error "internal error: undefined variable"

-- | Evaluate an expression to an integer.
evalInt :: Expr -> Env Val -> Int
evalInt e m = case evalExpr e m of
                Left i  -> i
                Right _ -> error "internal error: expected Int got Bool"

-- | Evaluate an expression to a Boolean.
evalBool :: Expr -> Env Val -> Bool
evalBool e m = case evalExpr e m of
                 Right b -> b
                 Left _  -> error "internal error: expected Bool got Int"

-- | Semantics of statements.
evalStmt :: Stmt -> Env Val -> Env Val
evalStmt (Bind x e)   m = insert x (evalExpr e m) m
evalStmt (If c st se) m = let Right b = evalExpr c m
                          in if b then evalStmt st m
                                  else evalStmt se m
evalStmt (While c sb) m =
    let Right b = evalExpr c m
    in if b then evalStmt (While c sb) (evalStmt sb m)
            else m
evalStmt (Block ss)   m = evalStmts ss m

evalStmts :: [Stmt] -> Env Val -> Env Val
evalStmts []     m = m
evalStmts (s:ss) m = evalStmts ss (evalStmt s m)


-- | Semantics of programs.
evalProg :: Prog -> Env Val
evalProg (P ds s) = evalStmt s m
  where
    m = fromList (map (\(x,t) -> (x, init t)) ds)
    init TInt  = Left 0
    init TBool = Right False

-- | Check and run a program.
runProg :: Prog -> Maybe (Env Val)
runProg p = if typeProg p then Just (evalProg p)
                          else Nothing
