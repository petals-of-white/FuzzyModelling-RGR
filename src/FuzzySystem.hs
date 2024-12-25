module FuzzySystem where
import           Core
import           Huzzy.Base.Sets
import           Huzzy.TypeOne.Sets

type X = Double

smallerX, smallX, largeX, largerX :: T1Set X
smallerX = uncurry contT1 xDomain 0.1 (tri 9.5 11 12.5)
smallX = uncurry contT1 xDomain 0.1 (tri 12.5 14 15.5)
largeX = uncurry contT1 xDomain 0.1 (tri 15.5 17 18.5)
largerX = uncurry contT1 xDomain 0.1 (tri 18.5 20 21.5)

type Y = Double
smallY, mediumY, largeY :: T1Set Y
smallY = uncurry contT1 yDomain 0.1 (tri 0.1 1.6 3.1)
mediumY = uncurry contT1 yDomain 0.1 (tri 2.8 3.1 3.4)
largeY = uncurry contT1 yDomain 0.1 (tri 3.1 4.6 5.1)

type Z = Double
negativeZ, zeroZ, positiveZ :: T1Set Z
negativeZ = uncurry contT1 zDomain 0.1 (down (-1) 0)
zeroZ = uncurry contT1 zDomain 0.1 (tri (-0.2) 0 0.2)
positiveZ = uncurry contT1 zDomain 0.1 (up 0 1)
