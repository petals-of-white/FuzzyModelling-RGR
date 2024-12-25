module FuzzySystem where
import           Core
import           Huzzy.Base.Sets
import           Huzzy.TypeOne.Sets
import           Huzzy.TypeOne.Systems

type X = Double

smallerX, smallX, largeX, largerX :: T1Set X
smallerX = uncurry contT1 xDomain 0.1 (tri 8.7 10.7 12.7)
smallX = uncurry contT1 xDomain 0.1 (tri 12.3 14.3 16.3)
largeX = uncurry contT1 xDomain 0.1 (tri 15.9 17.9 19.9)
largerX = uncurry contT1 xDomain 0.1 (tri 19.5 21.5 23.5)

type Y = Double
smallY, mediumY, largeY :: T1Set Y
smallY = uncurry contT1 yDomain 0.1 (tri 0.1 1.6 3.1)
mediumY = uncurry contT1 yDomain 0.1 (tri 2.0 3.1 4.2)
largeY = uncurry contT1 yDomain 0.1 (tri 3.1 4.6 6.1)

type Z = Double
negativeZ, zeroZ, positiveZ :: T1Set Z
negativeZ = uncurry contT1 zDomain 0.1 (down (-1) 0)
zeroZ = uncurry contT1 zDomain 0.1 (tri (-0.3) 0 0.3)
positiveZ = uncurry contT1 zDomain 0.1 (up 0 1)


accumulate :: Fuzzy a => [a] -> a
accumulate = foldr1 (?||)


fuzzyZ :: Double -> Double -> T1Set Z
fuzzyZ x y = accumulate (makeRules x y)

approxZ :: Double -> Double -> Double
approxZ x y = centroid (fuzzyZ x y)


makeRules :: Double -> Double -> [T1Set Z]
makeRules x y =
    [
        y `is` mediumY =*> zeroZ,
        x `is` smallerX ?&& y `is` smallY  =*> negativeZ,
        (x `is` smallerX ?&& y `is` largeY) `weight` 0.8  =*> positiveZ,
        x `is` smallX ?&& y `is` smallY =*> positiveZ,
        (x `is` smallX ?&& y `is` largeY) `weight` 0.8 =*> negativeZ,
        x `is` largeX ?&& y `is` smallY =*> negativeZ,
        (x `is` largeX ?&& y `is` largeY) `weight` 0.8 =*> positiveZ,
        x `is` largerX ?&& y `is` smallY =*> positiveZ,
        (x `is` largerX ?&& y `is` largeY) `weight` 0.8 =*> negativeZ
    ]
