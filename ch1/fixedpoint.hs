-- fixedpoint demonstrates fixed point algorithms

closeEnough :: Double -> Double -> Bool
closeEnough v1 v2 = abs(v1-v2) < tolerance
    where tolerance = 0.00001

fixedPoint :: (Double -> Double) -> Double -> Double
fixedPoint f firstGuess = try firstGuess
    where try guess = if closeEnough guess (f guess) then f guess 
                                                     else try (f guess)

averageDamp :: (Double -> Double) -> (Double -> Double)
averageDamp f = \x -> (x + f x)/2

mySqrt :: Double -> Double
mySqrt x = fixedPoint (averageDamp (\y -> x/y)) 1.0

main :: IO ()
main = do
    putStrLn $ show $ fixedPoint cos 1.0
    putStrLn $ show $ fixedPoint (\y -> sin y + cos y) 1.0
    putStrLn $ show $ mySqrt 2
