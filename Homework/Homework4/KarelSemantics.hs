-- Christian Mello (melloc), Steven Powers (powersst), Erik Watterson (watterse)

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
stmt Shutdown       _ _ r = Done r
stmt Move           _ w r = let p = getPos r
                            in if isClear (neighbor (getFacing r) p) w
                                then OK w (setPos (neighbor (getFacing r) p) r)
                                else Error ("Blocked at: " ++ show (neighbor (getFacing r) p))
stmt PickBeeper     _ w r = let p = getPos r
                            in if hasBeeper p w
                                then OK (decBeeper p w) (incBag r)
                                else Error ("No beeper to pick at: " ++ show p)
stmt PutBeeper      _ w r = let p = getPos r
                            in if not (isEmpty r)
                                then OK (incBeeper p w) (decBag r)
                                else Error "No beeper to put."
stmt (Turn d)       _ w r = OK w (setFacing (cardTurn d (getFacing r)) r)
stmt (Call m)       d w r = case lookup m d of
                                (Just b) -> stmt b d w r
                                _        -> Error ("Undefined macro: " ++ m)
stmt (Iterate i s)  d w r = if i > 0 then case stmt s d w r of
                                        (OK w' r') -> stmt (Iterate (i-1) s) d w' r'
                                        (Done r') -> Done r'
                                        (Error m) -> Error m
                                     else OK w r

stmt (If t st se)   d w r = let b = test t w r
                            in if b then stmt st d w r
                                    else stmt se d w r
stmt (While t sb)   d w r = if test t w r then case stmt sb d w r of
                                        (OK w' r') -> stmt (While t sb) d w' r'
                                        (Done r') -> Done r'
                                        (Error m) -> Error m
                                     else OK w r
stmt (Block [])     _ w r = OK w r
stmt (Block (s:ss)) d w r = case stmt s d w r of
                              (OK w' r') -> stmt (Block ss) d w' r'
                              (Done r') -> Done r'
                              (Error m) -> Error m

-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
