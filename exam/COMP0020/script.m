|| Environment:
|| 2.066 last revised 31 January 2020
|| compiled: Sat Apr 24 03:23:08 PM UTC 2021
|| gcc82 -w
|| host: x86_64 Linux 5.8.15-301.fc33.x86_64
|| gcc82 (GCC) 8.2.0
|| XVERSION 83

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
||                  λf.λx. n (λg.(λh.h (g f))) (λu.x) (λu.u)
||                                  p             q      r
||
|| Special Case
|| Let n = zero, as specified above, zero in church numeral is λf.λx.x
|| λf.λx. n (λg.(λh.h (g f))) (λu.x) (λu.u) ->
|| λf.λx. λf.λx.x (λg.(λh.h (g f))) (λu.x) (λu.u) ->
||     β-reducing (λg.(λh...)), replacing 0 times,
|| λf.λx. λx.x (λu.x) (λu.u) ->
|| λf.λx. (λu.x) (λu.u) ->
|| λf.λx. x
||     And that is identical to zero in church numeral
||     We proved f9 zero = zero
||
|| Normal Cases
||
|| Without the loss of generality, let n = two
|| λf.λx. two (λg.(λh.h (g f))) (λu.x) (λu.u) ->
|| λf.λx. λg.(λh.h (g f))) (one (λg.(λh.h (g f)))(λu.x)) (λu.u) ->
||     Using p, q, r as abbreviations below,
|| λf.λx. p (one p q) r ->
|| λf.λx. p (p (zero p q)) r ->
|| λf.λx. p (p (λf.λx.x (λg.(λh.h (g f))) (λu.x))) r ->
|| λf.λx. p (p (λx.x (λu.x))) r ->
||     As shown above in "special case", we remove one application of f at
||     this step
|| λf.λx. p (p (λu.x)) r ->
||     Substituting p with its λ expression, and using β-reduction to replace g
|| λf.λx. p ((λg.(λh.h (g f))) (λu.x)) r ->
|| λf.λx. p (λh.h ((λu.x) f)) r ->
|| λf.λx. p (λh.h x) r ->
||     Similarly,
|| λf.λx. ((λg.(λh.h (g f))) (λh.h x) r ->
|| λf.λx. λh.h ((λh.h x) f) r ->
|| λf.λx. λh.h (f x) r ->
||     Substituting r,
|| λf.λx. λh.h (f x) (λu.u) ->
|| λf.λx. (λu.u) (f x) ->
|| λf.λx. f x
||     And that is one in church numeral
||     We showed f9 two = one
|| The normal cases apply recursively upwards to all church numeral values.
||
|| In conclusion, the church predecessor function reduces the f application by
|| 1 time (when n != 0), by expanding the church numeral n to
|| "p(p...(zero p q)) r" (with p stacking up for n times). Since "zero p q"
|| removes a layer of f's application, reassembling it back would produce
|| church numeral n's predecessor.



|| Question 2

|| Definitions
|| Note: these definitions are of high level abstraction of the computer memory

|| A block is comprised of:
|| block header:
||    * num, the block size
||    * bool, the used/unused flag (for implcit free list)
||    * bool, the mark bit used for garbage collection
|| block memory:
||    * [num], list of pointers to other blocks
block == (num, bool, bool, [num])

|| A heap is comprised of:
||    * [block], many blocks
|| An empty heap is comprised of one single block:
||    * [(size, False, False, [0,0..])]
heap == [block]

|| Utility Functions
lastN :: num -> [*] -> [*]
lastN n xs = drop (#xs - n) xs

lookup :: num -> [*] -> *
lookup n [] = error "index out of bounds"
lookup 0 (x:xs) = x
lookup n (x:xs) = lookup (n - 1) xs

|| Main Functions

|| top-level memory allocation function, includes two steps:
||    * gc h, garbage collect the heap
||    * alloc x h, allocate new memory on the heap
malloc :: num -> heap -> heap
malloc x h = alloc x (gc h)

|| garbage collector - mark scan gc
||    * mark h h, first mark all used memory, two copies of h, one for
||        traversing the heap, one for preserving the marked heap
||    * scan h, then scan to garbage collect unused ones
gc :: heap -> heap
gc h = scangc (mark h h)

|| garbage collector -- marking stage

|| mark/unmark a single block
markBlock :: block -> block
markBlock (s, f, mb, d) = (s, f, True, d)

unmarkBlock :: block -> block
unmarkBlock (s, f, mb, d) = (s, f, False, d)

|| mark/unmark the nth block according to the pointer
markNthBlock :: num -> heap -> heap
markNthBlock n h = (take n h) ++ [markBlock (lookup n h)] ++ (drop (n + 1) h)

unmarkNthBlock :: num -> heap -> heap
unmarkNthBlock n h = (take n h) ++ [unmarkBlock (lookup n h)] ++ (drop (n + 1) h)

|| mark/unmark a list of blocks in heap according the pointer list
markBlockList :: [num] -> heap -> heap
markBlockList [] h = h
markBlockList (n:ns) h = markBlockList ns (markNthBlock n h)

unmarkBlockList :: [num] -> heap -> heap
unmarkBlockList [] h = h
unmarkBlockList (n:ns) h = unmarkBlockList ns (unmarkNthBlock n h)

|| mark all used blocks in a heap, we use two heaps here as:
||    * heap1 : traversing the blocks looking at each block's pointers if they
||              are in use
||    * heap2 : the marked heap
mark :: heap -> heap -> heap
mark [] h = h
mark ((s, f, mb, d):heapleft) h = mark heapleft (markBlockList d h), if f = True
                                = mark heapleft h, otherwise

|| garbage collector -- scanning stage

|| free the unmarked blocks, also unmark all the marked bits on the fly
free :: heap -> heap
free [] = []
free ((s, f, mb, d):heapleft) = (s, False, False, d):(free heapleft), if mb = False
                              = (s, f, False, d):(free heapleft), otherwise

|| coalesce the free blocks to reduce internal fragmentation
||    * if there are two free blocks, coalesce to make one free block
coalesce :: heap -> heap
coalesce [] = []
coalesce [b] = [b]
coalesce ((s1, f1, mb1, d1):(s2, f2, mb2, d2):heapleft) = (s1 + s2, False, False, d1 ++ d2):(coalesce heapleft), if f1 = False & f2 = False
                                                        = [(s1, f1, mb1, d1), (s2, f2, mb2, d2)] ++ coalesce heapleft, otherwise

|| scan all blocks in the heap, free unused ones and coalesce them
scangc :: heap -> heap
scangc h = coalesce (free h)

|| allocator

|| alloc function traverse the heap  using the implicit free list
|| the allocation policy is first-fit
alloc :: num -> heap -> heap
alloc 0 h = h
alloc x [(s, True, mb, d)] = error "no heap space left"
alloc x [(s, False, mb, d)] = [(x, True, mb, [0 | c <- [0..x - 1]]), (s - x, False, mb, lastN (s - x) d)], if s > x
                            = [(x, True, mb, [0 | c <- [0..x - 1]])], if s = x
                            = error "no heap space left", otherwise
alloc x ((s, True, mb, d):heapleft) = [(s, True, mb, d)] ++ alloc x heapleft
alloc x ((s, False, mb, d):heapleft) = [(x, True, mb, [0 | c <- [0..x - 1]]), (s - x, False, mb,  lastN (s - x) d)] ++ heapleft, if s > x
                                     = [(x, True, mb, [0 | c <- [0..x - 1]])] ++ heapleft, if s = x
                                     = [(s, False, mb, d)] ++ alloc x heapleft, otherwise

|| Tests

|| This is the top-level test function. Evaluate this variable to run all tests.
testall :: bool
testall = emptyheapAlloc6Test &
          emptyheapAlloc16Test &
          emptyheapAlloc0Test &
          usedheapAlloc2Test &
          usedheapAlloc5Test &
          usedheapAlloc6Test &
          garbageheapAlloc2Test &
          garbageheapAllocConsec23Test &
          garbageheapAllocConsec231Test

|| This is an empty heap
emptyheap :: heap
emptyheap = [(16, False, False, [0 | c <- [0..16 - 1]])]

|| This is a used heap, but no garbage blocks, as 1, 3 are referenced and used
usedheap :: heap
usedheap = [(3, False, False, [0 | c <- [0..3 - 1]]),
            (4, True, False,  [3, 3, 1, 3]),
            (6, False, False, [0 | c <- [0..6 - 1]]),
            (3, True, False,  [1, 3, 3])]

|| This is a garbage heap, only 1 is referenced and used, block 0, 2 are garbage
garbageheap :: heap
garbageheap = [(3, True, False,   [1, 1, 1]),
               (10, True, False,  [1 | c <- [0..10 - 1]]),
               (3, True, False,   [1, 1, 1])]


|| Test Case 1 - Empty Heap Allocation 6 bytes
emptyheapAlloc6 :: heap
emptyheapAlloc6 = [(6, True, False, [0, 0, 0, 0, 0, 0]), (10, False, False, [0 | c <- [0..10 - 1]])]

emptyheapAlloc6Test :: bool
emptyheapAlloc6Test = (malloc 6 emptyheap) == emptyheapAlloc6

|| Test Case 2 - Empty Heap Allocation 16 bytes - full
emptyheapAlloc16 :: heap
emptyheapAlloc16 = [(16, True, False, [0 | c <- [0..16 - 1]])]

emptyheapAlloc16Test :: bool
emptyheapAlloc16Test = (malloc 16 emptyheap) == emptyheapAlloc16

|| Test Case 3 - Empty Heap Allocation 0 bytes
emptyheapAlloc0Test :: bool
emptyheapAlloc0Test = (malloc 0 emptyheap) == emptyheap

|| Test Case 4 - Used Heap Allocation 2 bytes
usedheapAlloc2 :: heap
usedheapAlloc2 = [(2, True, False,  [0, 0]),
                  (1, False, False, [0]),
                  (4, True, False,  [3, 3, 1, 3]),
                  (6, False, False, [0 | c <- [0..6 - 1]]),
                  (3, True, False,  [1, 3, 3])]

usedheapAlloc2Test :: bool
usedheapAlloc2Test = (malloc 2 usedheap) == usedheapAlloc2

|| Test Case 5 - Used Heap Allocation 5 bytes
usedheapAlloc5 :: heap
usedheapAlloc5 = [(3, False, False, [0, 0, 0]),
                  (4, True, False,  [3, 3, 1, 3]),
                  (5, True, False,  [0, 0, 0, 0, 0]),
                  (1, False, False, [0]),
                  (3, True, False,  [1, 3, 3])]

usedheapAlloc5Test :: bool
usedheapAlloc5Test = (malloc 5 usedheap) == usedheapAlloc5

|| Test Case 6 - Used Heap Allocation 6 bytes
usedheapAlloc6 :: heap
usedheapAlloc6 = [(3, False, False, [0, 0, 0]),
                  (4, True, False,  [3, 3, 1, 3]),
                  (6, True, False,  [0, 0, 0, 0, 0, 0]),
                  (3, True, False,  [1, 3, 3])]

usedheapAlloc6Test :: bool
usedheapAlloc6Test = (malloc 5 usedheap) == usedheapAlloc5

|| Test Case 7 - Garbage Heap Allocation 0 bytes (expect garbage collected)
garbageheapAlloc0 :: heap
garbageheapAlloc0 = [(3, False, False,  [1, 1, 1]),
                     (10, True, False,  [1 | c <- [0..10 - 1]]),
                     (3, False, False,  [1, 1, 1])]

garbageheapAlloc0Test :: bool
garbageheapAlloc0Test = (malloc 0 garbageheap) == garbageheapAlloc0

|| Test Case 8 - Garbage Heap Allocation 2 bytes
garbageheapAlloc2 :: heap
garbageheapAlloc2 = [(2, True, False,   [0, 0]),
                     (1, False, False,  [1]),
                     (10, True, False,  [1 | c <- [0..10 - 1]]),
                     (3, False, False,  [1, 1, 1])]

garbageheapAlloc2Test :: bool
garbageheapAlloc2Test = (malloc 2 garbageheap) == garbageheapAlloc2

|| Test Case 9 - Garbage Heap Consecutive Allocation 3 bytes (following Test 8)
garbageheapAllocConsec23 :: heap
garbageheapAllocConsec23 = [(2, True, False,   [0, 0]),
                            (1, False, False,  [1]),
                            (3, True, False,   [0, 0, 0]),
                            (10, False, False, [1 | c <- [0..10 - 1]])]

garbageheapAllocConsec23Test :: bool
garbageheapAllocConsec23Test = (malloc 3 (malloc 2 garbageheap)) == garbageheapAllocConsec23

|| Test Case 10 - Garbage Heap Consecutiv Allocation 1 bytes (following Test 9)
garbageheapAllocConsec231 :: heap
garbageheapAllocConsec231 = [(2, True, False,   [0,0]),
                             (1, True, False,   [0]),
                             (13, False, False, [0,0,0,1,1,1,1,1,1,1,1,1,1])]
garbageheapAllocConsec231Test :: bool
garbageheapAllocConsec231Test = (malloc 1 (malloc 3 (malloc 2 garbageheap))) == garbageheapAllocConsec231
