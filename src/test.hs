data MaybeNew x  = None | Some x

ifNone None = 0
ifNone (Some x) = x

main= print (ifNone (None))
