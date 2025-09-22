expC :: Double -> [Double]
expC x = [ (x ^ i) / ((1:fact 2 1) !! i) | i <- [0..]]

fact :: Num t => t -> t -> [t]
fact s n =  n:fact (s + 1) (n * s)

exp' :: Double -> Int -> Double
exp' x n = sum(take (n + 1) (expC x))

main :: IO()
main = do
    print("#5-1: ", take 10 (expC 10))
    print("#5-2: ", exp' 10 50)