module Plot where
import           Core
import           FuzzySystem
import           Graphics.EasyPlot
import           Huzzy.Base.Sets   (is)

plotXTerms, plotYTerms, plotZTerms :: TerminalType -> IO Bool

plotXTerms terminal = plot' [Interactive] terminal subplots
    where subplots = [
            Function2D [Title "smaller"] [range] (`is` smallerX),
            Function2D [Title "small"] [range] (`is` smallX),
            Function2D [Title "large"] [range] (`is` largeX),
            Function2D [Title "larger"] [range] (`is` largerX)]
          range = uncurry Range xDomain


plotYTerms terminal = plot' [Interactive] terminal subplots
    where subplots = [
            Function2D [Title "small"] [range] (`is` smallY),
            Function2D [Title "medium"] [range] (`is` mediumY),
            Function2D [Title "large"] [range] (`is` largeY)]
          range = uncurry Range yDomain



plotZTerms terminal = plot' [Interactive] terminal subplots
    where subplots = [
            Function2D [Title "negative"] [range] (`is` negativeZ),
            Function2D [Title "zero"] [range] (`is` zeroZ),
            Function2D [Title "positive"] [range] (`is` positiveZ)]
          range = uncurry Range zDomain


plotZ :: TerminalType -> IO Bool
plotZ terminalType = plot' [Interactive] terminalType (Function3D [] [RangeX xStart xEnd, RangeY yStart yEnd] z)
    where
        (xStart, xEnd) = xDomain
        (yStart, yEnd) = yDomain

