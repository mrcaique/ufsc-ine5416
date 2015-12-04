import Data.Char
import System.Environment
import System.IO

-- principal module
main::IO ()
main = do
    args <- getArgs
    case args of
        [input] -> do
            processFile input

-- Separating RGB layers
splitRGB (b:g:r:[]) = ([b], [g], [r])
splitRGB (b:g:[]) = ([b], [g], [0])
splitRGB (b:[]) = ([b], [0], [0])
splitRGB (b:g:r:rest) =
    let (bs, gs, rs) = splitRGB rest
    in (b:bs, g:gs, r:rs)

-- Get the floating point from a division
fpoint :: Int -> Float
fpoint i = fromIntegral i / 256

-- Converts a tuple to a list
toList :: (a, a , a) -> [a]
toList (x, y, z) = [x, y, z]

-- Main function, processes the file
processFile input = do
    toString <- readFile input
    
    let file_name = take (length input - 4) input
    let output_name = map (file_name++) ["_blue.out", "_green.out", "_red.out"]
    outputs <- sequence [openFile f WriteMode | f <- output_name]
    
    let deep = snd (splitAt 54 (map ord toString))
    let rgb = toList (splitRGB (map fpoint deep))
    sequence [hPrint file color | (file, color) <- zip outputs rgb]
    sequence [hClose f | f <- outputs]
    return()
