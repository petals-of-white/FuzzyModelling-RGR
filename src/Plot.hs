module Plot where
import Core
import Graphics.EasyPlot

plotZ :: TerminalType -> IO Bool
plotZ terminalType = plot' [Interactive] terminalType z
