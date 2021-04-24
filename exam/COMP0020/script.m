f :: num -> num
f x = x * 2

zero :: (* -> *) -> * -> *
zero f x = x

one :: (* -> *) -> * -> *
one f x = f x

two :: (* -> *) -> * -> *
two f x = f (f x)

three :: (* -> *) -> * -> *
three f x  = f (f (f x))

|| 1.a
plus :: ((* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *
plus a b f x = a f (b f x)


|| 1.b
times :: ((* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> ((* -> *) -> * -> *)
times a b = a . b


|| 1.c

f9 n = g
       where
       g f x = n p q r
               where
               p g h = h (g f)
               q u = x
               r u = u
