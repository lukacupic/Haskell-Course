-- =============================================================================== --
{- |
  Welcome to your fifth Haskell training. Get ready to rumble.
  Where will you let recursion lead you?

  You should know the drill by now - solve the exercises in `TrainingExercises.hs`,
  run the tests with Cabal, push to `training-05`, create a Merge Request,
  and assign it to your TA.

  Keep in mind that **all exposed functions must have type signatures** (helper functions that are defined in local definitions don't)!

  As always, ask your TA if you need any help.
-}
-- =============================================================================== --
--
module TrainingExercises where
--
import Data.List
import Data.Char
--

{- * 5.1 Recursive functions with accumulators -}

{-
 - For the following functions, we'd like you to to use the `seq` function to
 - reduce the accumulators to WHNF where needed to avoid building big thunks.
 -}

-- ** TE 5.1.1
--
-- | Define a recursive function with accumulation which finds the
-- | first longest word in a sentence.
-- | Make sure you keep track of the length of the currently longest word
-- | too, so you don't call `length` repeatedly on the same word.

te511 :: String -> String
te511 ss = te511' wurds (head wurds) $ length $ head wurds
    where wurds = words ss
          te511' [] os len = os
          te511' (s:ss) os len
            | length s > len = te511' ss s $ length s
            | otherwise      = te511' ss os len

-- ** TE 5.1.2
--
-- | Define a recursive function with accumulation which takes a list of
-- | polynomial coefficients and a variable, and calculates the polynomial using
-- | Horner's method (https://en.wikipedia.org/wiki/Horner%27s_method ).
-- | The coefficients are in descending order of variable exponents, so a list
-- | representing the polynomial x^3 - 2x + 3 would be  as  [1, 0, -2, 3].

te512 :: Num a => [a] -> a -> a
te512 ccs x = horner ccs x 0
    where horner [] _ res     = res
          horner (c:cs) x res = horner cs x $ res `seq` (c + res*x)

-- ** TE 5.1.3
--
-- | Define a function which computes the population standard deviation of a list of
-- | numbers. To achieve this you need to compute the mean and variance of the list:
-- | do this using recursive functions with accumulation.

te513 :: Floating a => [a] -> a
te513 xs = sqrt $ (stdsum xs m 0) / len
    where len = realToFrac $ length xs
          m   = mean xs len 0

stdsum :: Floating a => [a] -> a -> a -> a
stdsum (x:[]) m res = res + (x - m)**2
stdsum (x:xs) m res = stdsum xs m res'
    where res' = res `seq` res + (x - m)**2

mean :: Floating a => [a] -> a -> a -> a
mean (x:[]) len res = (res + x) / len
mean (x:xs) len res = res `seq` mean xs len (res + x)

-- ** TE 5.1.4
--
-- | An aspiring rollercoaster designer wants to test out his if his newest
-- | creation is safe to ride, and needs your help!
-- | Define a function which takes a list of pairs which describe a section of
-- | the track. The first element will be a String which will be either "even",
-- | "drop", "turn left" or "turn right". The second element will be a number.
-- |
-- | If it's a "drop", the car accelerates as if it was in free-fall (accelerating
-- | 9.81 m/s^2) and the number indicates the height of the drop in meters.
-- | The car maintains its current speed coming into the drop.
-- |
-- | If it's "even", the car decelerates by 0.5 m/s every meter it passes. The
-- | number indicates the length of the even segment. If the car decelerates to
-- | 0 km/s, the track is deemed unsafe as the passengers will become stuck!
-- |
-- | If it's either of the two "turn"s, the number indicates the radius of the
-- | turn in meters. The car will derail if it turns too tightly: it can only
-- | withstand centripetal acceleration of up to and including 5G. And if there
-- | are 3 or more alternating turns directly in a row, the passengers will become
-- | nauseous, which can be unsafe.
-- |
-- | The car starts moving at 20 km/h. The function must return a list indicating
-- | whether the rollercoaster is safe or not. If it is safe, it returns an empty list.
-- | If the rollercoaster is not safe, it returns a list with one element: the
-- | index of the segment of the track where it becomes unsafe.
-- | (Later on, you will learn a much more elegant way of representing a result
-- | which might contain a value, or might contain nothing at all, but this will
-- | do for now.)

te514 :: [(String, Double)] -> [Int]
te514 [] = error "The list cannot be empty!"
te514 ps = ride ps (20/3.6) (0, 0)

ride :: [(String, Double)] -> Double -> (Int, Int) -> [Int]
-- Safety check
ride _ _ (i, 3)      = [i - 1]
ride [] v (i, _)     = if v >= 0 then [] else [i - 1]
-- Physics
ride (("drop", d):ps) v (i, turns)            = ride ps (sqrt $ v**2 + 2 * 9.81 * d) (i + 1, turns)
ride (("even", d):ps) v (i, turns)            = ride ps (v - 0.5 * d) (i + 1, turns)
ride (('t':'u':'r':'n':_, r):ps) v (i, turns) = res
    where a   = v**2 / r
          res = if a <= 5 * 9.81 then ride ps v (i + 1, turns + 1) else [i]

-- ** TE 5.1.5 - EXTRA
--
-- | Define a recursive function with accumulation which computes the square root
-- | of a given number using Newton's method, with the given number of iterations.
-- | Use the halved original number as an initial guess for the method.
te515 :: (Ord a, Fractional a, Integral b) => a -> b -> a
te515 = undefined