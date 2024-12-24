module Main (main) where
import           Control.Monad     (void)
import           Graphics.EasyPlot (TerminalType (Windows))
import           Plot              (plotZ)

main :: IO ()
main = void $ plotZ Windows

