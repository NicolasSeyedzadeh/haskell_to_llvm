mul acc x 0 = acc
mul acc x y = mul (acc+x) (x) (y-1)
main= print (mul 0  12345 54321)
