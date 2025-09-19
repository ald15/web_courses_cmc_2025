or_ :: Bool -> Bool -> Bool
or_ False False = False
or_ _ _ = True 
-- Why _ symbol? May be other synbols?
main :: IO()
main = do
    print(or_ True False)

