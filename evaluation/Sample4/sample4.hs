data Maybe z = None | Some z

bind f None = None
bind f (Some val) = f val

safediv x 0 = None
safediv x y = Some (x/y)

ifNone None = print "error: divide by 0"
ifNone (Some x) = print x

main = ifNone (bind (safediv 25) (bind (safediv 40) (Some 8)))
