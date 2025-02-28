add a b = a + b
plus3 = add 3

main :: IO ()
main = print (plus3 5)


return :: a -> Maybe a
return x  = Just x

(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
(>>=) m g = case m of
                Nothing -> Nothing
                Just x  -> g x
