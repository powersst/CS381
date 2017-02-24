module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState


-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not t)    w r = not (test t w r)
test (Facing c) _ r = getFacing r == c
test (Clear d)  w r = isClear (relativePos d r) w
test Beeper     w r = hasBeeper (getPos r) w
test Empty      _ r = isEmpty r

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown      _ _ r = Done r
stmt Move          _ _ _ = undefined
-- stmt Move       _ w r = let p = getPos r
--                         in if isClear p w
--                               then OK _ (setPos)
stmt PickBeeper    _ w r = let p = getPos r
                            in if hasBeeper p w
                                then OK (decBeeper p w) (incBag r)
                                else Error ("No beeper to pick at: " ++ show p)
stmt PutBeeper     _ w r = let p = getPos r
                            in if not (isEmpty r)
                                then OK (incBeeper p w) (decBag r)
                                else Error "No beeper in bag!"
stmt (Turn d)      _ w r = OK w (setFacing (cardTurn d (getFacing r)) r)
stmt (Call _)      _ _ _ = undefined
stmt (Iterate _ _) _ _ _ = undefined
stmt (If t st se)  d w r = let b = test t w r
                            in if b then stmt st d w r
                                    else stmt se d w r
stmt (While t sb)  d w r = let b = test t w r
                            in if b then stmt (While t sb) d w r
                                    else OK w r
-- stmt (Block [])    _ w r = OK w r
-- stmt (Block (h:t)) d w r = case

-- | Run a Karel program.
--prog :: Prog -> World -> Robot -> Result
--prog (m,s) w r = stmt s m w r
