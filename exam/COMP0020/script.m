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

|| This is the church predecessor function.
|| f9 <church-numeral n> = <church-numeral 0>,     if n = 0
||                       = <church-numeral n - 1>, otherwise
||
|| First the function describes:
||                                  f9 n = g
||
|| n is the church numeral we are looking to reduce by 1 (or not if n = zero)
|| , and "f9 n = g" means that: g is n's predecessor.
||
|| To elaborate on g:
||                                g f x = n p q r
|| g f x is applying the church numeral to a concrete f and x. We expect g f x
|| to be analogous to "pred(n) f x", but we have to do it in a lambda calculus
|| way. Hence we let g f x = n p q r, applying n to three arguments in an
|| attmept to reduce n.
||
|| The first argument, p g h = h (g f) can be written as
||                               λg.(λh.h (g f))
|| This function takes the current numeral as g, and return a new function that
|| is g applied to f. The function essentially "dissects" n.
||
|| The second argument, is what makes the function applies one less.
|| q u = x can be written as
||                                    λu.x
|| This function is a constant function, saying that anything would evaluate to
|| the original value x. We can use it to reduce a "layer" of f's application
|| on x.
||
|| The third argument, r u = u is simply the identity function
||                                    λu.u
|| that returns the function as-is.
||
||
|| We can further explain how predecessor works via examples:
||
|| Rewriting n p q r into λ expression:
||                      n (λg.(λh.h (g f))) (λu.x) (λu.u)
||                               p             q      r
||
|| Special Case
|| Let n = zero, as specified above, zero in church numeral is λf.(λx.x)
|| n (λg.(λh.h (g f))) (λu.x) (λu.u) ->
|| λf.(λx.x) (λg.(λh.h (g f))) (λu.x) (λu.u) ->
||     β-reducing (λg.(λh...)), replacing 0 times,
|| λf.(λx.x) (λu.x) (λu.u) ->
|| λf.(λu.x) (λu.u) ->
|| λf.(λx.x)
||     And that is identical to zero in church numeral
||     We proved f9 zero = zero
||
|| Normal Cases
||
|| Without the loss of generality, let n = two
|| two (λg.(λh.h (g f))) (λu.x) (λu.u) ->
|| (λg.(λh.h (g f)))(one (λg.(λh.h (g f)))(λu.x))(λu.u) ->
||     Using p, q, r as abbreviations below,
|| p (one p q) r ->
|| p (p (zero p q)) r ->
||     As shown above in "special case", we remove one application of f at
||     this step
|| p (p (λu.x)) r ->
||     Substituting p with its λ expression, and using β-reduction to replace g
|| p (λh.h ((λu.x) f)) r ->
|| p (λh.h x) r ->
||     Similarly,
|| λh.h ((λh.h x) f) r ->
|| λh.h (f x) r ->
||     Substituting r,
|| λh.h (f x) (λu.u) ->
|| (λu.u) (f x) ->
|| f x
||     And that is one in church numeral
||     We showed f9 two = one
|| The normal cases apply recursively upwards to all church numeral values.
||
|| In conclusion, the church predecessor function reduces the f application by
|| 1 time (when n != 0), by expanding the church numeral n to
|| "p(p...(zero p q)) r" (with p stacking up for n times). Since "zero p q"
|| would be the constant x, reassembling it back would produce church numeral
|| n's predecessor.



|| Question 2


|| Definitions

block == (num, bool, [num])

heap == [block]


|| Allocator

lastN :: num -> [*] -> [*]
lastN n xs = drop (#xs - n) xs

malloc :: num -> heap -> heap
malloc 0 h = h
malloc x [(s, True, d)] = error "no heap space left"
malloc x [(s, False, d)] = [(x, True, [0 | c <- [0..x - 1]]), (s - x, False, lastN (s - x) d)], if s > x
                         = [(x, True, [0 | c <- [0..x - 1]])], if s = x
                         = error "no heap space left", otherwise
malloc x ((s, True, d):heapleft) = [(s, True, d)] ++ malloc x heapleft
malloc x ((s, False, d):heapleft) = [(x, True, [0 | c <- [0..x - 1]]), (s - x, False, lastN (s - x) d)] ++ heapleft, if s > x
                                  = [(x, True, [0 | c <- [0..x - 1]])] ++ heapleft, if s = x
                                  = [(s, False, d)] ++ malloc x heapleft, otherwise


|| Test
emptyheap :: heap
emptyheap = [(16, False, [0 | c <- [0..16 - 1]])]

test_naive_malloc :: heap
test_naive_malloc = malloc 2 emptyheap

test_second_malloc :: heap
test_second_malloc = malloc 5 test_naive_malloc

usedheap :: heap
usedheap = [(3, False, [0 | c <- [0..3 - 1]]), (4, True, [1, 2, 3, 4]), (6, False, [0 | c <- [0..6 - 1]]), (3, True, [7, 8, 9])]
