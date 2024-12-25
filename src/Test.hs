module Test where

import           Core          (xDomain, yDomain, z)
import           FuzzySystem
import           System.Random

testApproxZ :: StdGen -> ([((Double, Double), (Double, Double))], Double)
testApproxZ g =
    let testPoints = makeTestPoints g
        results = [(z x y, approxZ x y) | (x,y) <- testPoints]
        err = meanSquareError results
 
    in (zip testPoints results, err)

makeTestPoints :: StdGen -> [(Double, Double)]
makeTestPoints g =
    let (gen1, gen2) = split g
    in take 50 $ zip (randomRs xDomain gen1) (randomRs yDomain gen2)



meanSquareError :: Floating a => [(a,a)] -> a
meanSquareError input =
    let errors = [(a-b) ** 2 | (a,b) <- input]
    in sum errors / fromIntegral (length errors)
