data MaybeNew x  = None | Some x

ifNone None = 0
ifNone (Some x) = 1

main= print (ifNone (Some 3))
