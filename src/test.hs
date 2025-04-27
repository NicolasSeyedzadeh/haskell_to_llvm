data MaybeNew x y  = None | Some x y

ifNone None = 0
ifNone (Some x) = x

main= print (ifNone None)
