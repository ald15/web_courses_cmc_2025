sqRoots :: Double -> Double -> Double -> (Double, Double)
sqRoots a b c = let d = sqrt(b*b -4*a*c)
                    w = 2*a
                    in ((-b-d)/w, (-b+d)/w)

main :: IO()
main = do
    print(sqRoots 1 2 1)