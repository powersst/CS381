Given:

  not  :: Bool -> Bool
  even :: Int -> Bool
  map  :: (a -> b) -> [a] -> [b]
  (.)  :: (b -> c) -> (a -> b) -> a -> c

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g x = f (g x)


What is the type of the following expressions?
 
 * map 3

   map :: (a -> b) -> [a] -> [b]
     3 :: Int =? (a -> b)  NO!
   Type error! :-(

 * map even

   map :: (a -> b) -> [a] -> [b]
     even :: Int -> Bool =? a -> b  Yes: { a = Int, b = Bool }
   map even :: [Int] -> [Bool]

 * not . even
   (.) not even
     not :: Bool -> Bool =? b -> c  Yes: { b = Bool, c = Bool }
   (.) not :: (a -> Bool) -> a -> Bool
     even :: Int -> Bool =? a -> Bool  Yes: { a = Int }
   (.) not even :: Int -> Bool

 * even . not
   (.) even not
     even :: Int -> Bool =? b -> c  Yes: { b = Int, c = Bool }
   (.) even :: (a -> Int) -> a -> Bool
     not :: Bool -> Bool =? a -> Int  No!
   Type Error! :-(