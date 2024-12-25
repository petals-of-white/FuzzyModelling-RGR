module Plot where
import           Core
import           FuzzySystem
import           Graphics.EasyPlot
import           Huzzy.Base.Sets   (is)

plotXTerms, plotYTerms, plotZTerms :: TerminalType -> IO Bool

plotXTerms terminal = plot' [Interactive] terminal subplots
    where subplots = [
            Function2D [Title "X smaller"] [range] (`is` smallerX),
            Function2D [Title "X small"] [range] (`is` smallX),
            Function2D [Title "X large"] [range] (`is` largeX),
            Function2D [Title "X larger"] [range] (`is` largerX)]
          range = uncurry Range xDomain


plotYTerms terminal = plot' [Interactive] terminal subplots
    where subplots = [
            Function2D [Title "Y small"] [range] (`is` smallY),
            Function2D [Title "Y medium"] [range] (`is` mediumY),
            Function2D [Title "Y large"] [range] (`is` largeY)]
          range = uncurry Range yDomain



plotZTerms terminal = plot' [Interactive] terminal subplots
    where subplots = [
            Function2D [Title "Z negative"] [range] (`is` negativeZ),
            Function2D [Title "Z zero"] [range] (`is` zeroZ),
            Function2D [Title "Z positive"] [range] (`is` positiveZ)]
          range = uncurry Range zDomain


plotZ :: TerminalType -> IO Bool
plotZ terminalType = plot' [Interactive] terminalType (Function3D [Title "z(x,y)"] [RangeX xStart xEnd, RangeY yStart yEnd] z)
    where
        (xStart, xEnd) = xDomain
        (yStart, yEnd) = yDomain


plotApproxZ :: TerminalType -> IO Bool
plotApproxZ terminal =
  plot'
    [Interactive] terminal
    (Function3D [Title "fuzzy z(x,y)"] [uncurry RangeX xDomain, uncurry RangeY yDomain] approxZ)
