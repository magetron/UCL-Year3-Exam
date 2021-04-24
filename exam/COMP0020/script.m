|| Question 1

f :: num -> num
f x = x * 2

|| Church Numerals

zero :: (* -> *) -> * -> *
zero f x = x

one :: (* -> *) -> * -> *
one f x = f x

two :: (* -> *) -> * -> *
two f x = f (f x)

three :: (* -> *) -> * -> *
three f x  = f (f (f x))

|| 1.a Church Addition

plus :: (*** -> ** -> *) -> (*** -> **** -> **) -> *** -> **** -> *
plus a b f x = a f (b f x)

|| 1.b Church Multiplication
|| Alternative definition: times a b = a . b function composition

times :: (** -> *) -> (*** -> **) -> *** -> *
times a b f = a (b f)


|| 1.c

f9 n = g
       where
       g f x = n p q r
               where
               p g h = h (g f)
               q u = x
               r u = u

|| This is the Church predecessor function.
|| f9 <church-numeral n> = <church-numeral 0>,     if n = 0
||                       = <church-numeral n - 1>, otherwise
||
|| First the function describes:
||                                  f9 n = g
||
|| n is the church numeral we are looking to reduce by 1 (or not if n = zero)
|| , and `f9 n = g` means that: g is n's predecessor.
||
|| To elaborate on g:
||                                g f x = n p q r
|| g f x is applying the chruch numeral to a concrete f and x. We expect g f x
|| to be the analagous to "pred(n) f x", but we have to do it in a lambda
|| calculus way. Hence we let g f x = n p q r, applying n to three arguments in
|| an attmept to reduce n.
||
|| The first argument, p g h = h (g f) can be written as
||                               λg.(λh.h (g f))
|| This function takes the current numeral as g, and return a new function that
|| is g applied to f. The function essentially "dissects" n to (g f).
||
|| The second argument, is what makes the function applies one less.
|| q u = x can be written as
||                                    λu. x
|| This function is a constant function, saying that anything would evaluate to
|| the original value x. We can use it to reduce a "layer" of f's application
|| on x.
||
|| The third argument, r u = u is simply the identity function
||                                    λu. u
|| that returns the function as-is.
||
||
|| We can further explain how predecessor works via examples:
||
|| Rewriting n p q r into λ expression:
||                      n (λg.(λh.h (g f))) (λu. x) (λu. u)
|| Let n = 0:
||
||
