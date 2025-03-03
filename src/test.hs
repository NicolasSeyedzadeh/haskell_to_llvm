add a b = a + b
plus3 = add 3
main = print (plus3 5)

data MaybeNew x = None | Some x

instance Functor MaybeNew where
    fmap f None = None
    fmap f (Some x) = Some (f x)

instance Applicative MaybeNew where
  pure = Some
  None <*> _ = None
  _ <*> None = None
  Some f <*> Some x = Some (f x)

instance Monad MaybeNew where
    None >>= _ = None
    Some x >>= f = f x

x=None
