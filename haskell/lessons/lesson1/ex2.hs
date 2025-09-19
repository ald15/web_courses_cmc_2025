-- Define the factorial function
factorial :: Integer -> Integer
factorial n = if n == 0 then 1 else n * factorial (n - 1)

-- Main function to execute the program
main :: IO ()
main = do
    print(factorial 50000)
