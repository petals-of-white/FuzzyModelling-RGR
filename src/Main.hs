module Main (main) where
import           Graphics.EasyPlot (TerminalType (..))
import           Plot
import           System.Random     (newStdGen)
import           Test              (testApproxZ)
import           Text.Printf       (printf)


main :: IO ()
main =
    plotZ Windows
    >> plotXTerms Windows >> plotYTerms Windows >> plotZTerms Windows
    >> plotApproxZ Windows
    >> newStdGen >>= (showResults . testApproxZ)

showResults :: ([((Double, Double), (Double, Double))], Double) -> IO ()
showResults (res, meanSqureErr) =  do
    putStrLn $
        unlines
            [printf "x = %.2f, y = %.2f, z = %.2f, approxZ = %.2f" x y realZ approxZ
            | ((x,y), (realZ, approxZ)) <- res]

    putStrLn ("Mean square error: " ++ show meanSqureErr)

