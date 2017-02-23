module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState


-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test = undefined

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r
stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)
stmt PutBeeper  _ w r = let p = getPos r
                        in if not (isEmpty r)
                              then OK (incBeeper p w) (decBag r)
                              else Error "No beeper in bag!"

-- | Run a Karel program.
--prog :: Prog -> World -> Robot -> Result
--prog (m,s) w r = stmt s m w r
