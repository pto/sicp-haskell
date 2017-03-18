sqrt' :: (Fractional a, Ord a) => a -> a
sqrt' x = sqrtIter 1.0 x

sqrtIter :: (Fractional a, Ord a) => a -> a -> a
sqrtIter guess x = if goodEnough guess x
                        then guess
                        else sqrtIter (improve guess x) x

improve :: Fractional a => a -> a -> a
improve guess x = average guess (x / guess)

average :: Fractional a => a -> a -> a
average x y = (x + y) / 2

goodEnough :: (Fractional a, Ord a) => a -> a -> Bool
goodEnough guess x = abs (square guess - x) < 0.001

square :: Num a => a -> a
square x = x * x

display :: Show a => a -> IO ()
display x = putStrLn (show x)

main :: IO ()
main = do
    display $ sqrt' 9
    display $ sqrt' (100 + 37)
    display $ sqrt' (sqrt' 2 + sqrt' 3)
    display $ square (sqrt' 1000)
