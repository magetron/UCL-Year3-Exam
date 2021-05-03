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

|| Summary
|| In this question, we programmed a malloc function with mark-scan garbage
|| collection. The function also comes with all-round test cases.
||
|| For the allocator, the heap is traversed with a implicit free list, and the
|| allocation is based on a first-fit policy. The estimated time complexity for
|| the allocation alone is O(n) (n being the number of blocks in the heap).
||
|| For the garbage collector, the marking process is done by visiting every
|| single pointer within each active block. The time complexity is O(m) (m being
|| the number of active block bytes). The scanner includes both the free
|| function and the coalesce function. "free" would traverse through the
|| heap in O(n) time and free the unmarked blocks. "coalesce" then merges the
|| free blocks together in O(n) time to reduce internal fragmentation. In total,
|| the garbage collector operates on a linear O(m) time scale.
||
|| Testing is done via evaluating the "testall" variable. It includes several
|| test suites simulating use cases of empty heaps, used heaps and heaps with
|| garbage. We also test the consecutive allocation capability of the malloc
|| function with "malloc 3 (malloc 2 heap)".

|| Definitions
|| Note: these definitions are of high level abstraction of the computer memory

|| Each segment in user accessible malloc'd payload can be either:
||    * a pointer to an address
||    * a plain data number
data ::= Pointer num | Data num

|| A block is comprised of:
|| block header:
||    * num, the block address
||    * num, the block size
||    * bool, the used/unused flag (for implcit free list)
||    * bool, the mark bit used for garbage collection
|| block malloc'd payload:
||    * [data], list of data as specified above
block == (num, num, bool, bool, [data])

|| A heap is comprised of:
||    * [block], many blocks
|| An empty heap is comprised of one single block:
||    * [(addr, size, False, False, [Data 0, Data 0 ...])]
heap == [block]

|| Utility Functions
lastN :: num -> [*] -> [*]
lastN n xs = drop (#xs - n) xs

|| Main Functions

|| top-level memory allocation function, includes two steps:
||    * gc h, garbage collect the heap
||    * alloc x h, allocate new memory on the heap
malloc :: num -> heap -> heap
malloc x h = alloc x (gc h)

|| garbage collector - mark scan gc
||    * mark h h, first mark all used memory: two copies of h, one for
||        traversing the heap, one for preserving the marked heap
||    * scan h, then scan to garbage collect unused ones
gc :: heap -> heap
gc h = scangc (mark h h)

|| garbage collector -- marking stage

|| mark/unmark a single block
markBlock :: block -> block
markBlock (a, s, f, mb, d) = (a, s, f, True, d)

unmarkBlock :: block -> block
unmarkBlock (a, s, f, mb, d) = (a, s, f, False, d)

|| mark/unmark the address block according to the pointer given
|| ignore if data is plain data
markAddrBlock :: data -> heap -> heap
markAddrBlock (Data x) h = h
markAddrBlock (Pointer addr) [] = error "address not found"
markAddrBlock (Pointer addr) ((a, s, f, mb, d):heapleft) = (markBlock (a, s, f, mb, d)):heapleft, if addr = a
                                                         = (a, s, f, mb, d):(markAddrBlock (Pointer addr) heapleft), otherwise

unmarkAddrBlock :: data -> heap -> heap
unmarkAddrBlock (Data x) h = h
unmarkAddrBlock (Pointer addr) [] = error "address not found"
unmarkAddrBlock (Pointer addr) ((a, s, f, mb, d):heapleft) = (unmarkBlock (a, s, f, mb, d)):heapleft, if addr = a
                                                           = (a, s, f, mb, d):(unmarkAddrBlock (Pointer addr) heapleft), otherwise

|| mark/unmark a list of blocks in heap according the data list (with pointers)
markBlockList :: [data] -> heap -> heap
markBlockList [] h = h
markBlockList (a:as) h = markBlockList as (markAddrBlock a h)

unmarkBlockList :: [data] -> heap -> heap
unmarkBlockList [] h = h
unmarkBlockList (a:as) h = unmarkBlockList as (unmarkAddrBlock a h)

|| mark all used blocks in a heap, we use two heaps here as:
||    * heap1 : traversing the blocks looking at each block's pointers if they
||              are in use
||    * heap2 : the marked output heap
mark :: heap -> heap -> heap
mark [] h = h
mark ((a, s, f, mb, d):heapleft) h = mark heapleft (markBlockList d h), if f = True
                                   = mark heapleft h, otherwise

|| garbage collector -- scanning stage

|| free the unmarked blocks, also unmark all the marked bits on the fly
free :: heap -> heap
free [] = []
free ((a, s, f, mb, d):heapleft) = (a, s, False, False, d):(free heapleft), if mb = False
                                 = (a, s, f, False, d):(free heapleft), otherwise

|| coalesce the free blocks to reduce internal fragmentation
||    * if there are two free blocks, coalesce to make one free block, and
||      continue to do so recursively until all consecutive free blocks are
||      merged into one.
coalesce :: heap -> heap
coalesce [] = []
coalesce [b] = [b]
coalesce ((a1, s1, f1, mb1, d1):(a2, s2, f2, mb2, d2):heapleft) = coalesce ([(a1, s1 + s2, False, False, d1 ++ d2)] ++ heapleft), if f1 = False & f2 = False
                                                                = [(a1, s1, f1, mb1, d1)] ++ coalesce ([(a2, s2, f2, mb2, d2)] ++ heapleft), otherwise

|| scan all blocks in the heap, free unused ones and coalesce them
scangc :: heap -> heap
scangc h = coalesce (free h)

|| allocator

|| alloc function traverse the heap using the implicit free list provided in
|| blocks. The allocation policy is first-fit.
alloc :: num -> heap -> heap
alloc 0 h = h
alloc x [(a, s, True, mb, d)] = error "no heap space left"
alloc x [(a, s, False, mb, d)] = [(a, x, True, mb, [Data 0 | c <- [0..x - 1]]), (a + x, s - x, False, mb, lastN (s - x) d)], if s > x
                               = [(a, x, True, mb, [Data 0 | c <- [0..x - 1]])], if s = x
                               = error "no heap space left", otherwise
alloc x ((a, s, True, mb, d):heapleft) = [(a, s, True, mb, d)] ++ alloc x heapleft
alloc x ((a, s, False, mb, d):heapleft) = [(a, x, True, mb, [Data 0 | c <- [0..x - 1]]), (a + x, s - x, False, mb, lastN (s - x) d)] ++ heapleft, if s > x
                                        = [(a, x, True, mb, [Data 0 | c <- [0..x - 1]])] ++ heapleft, if s = x
                                        = [(a, s, False, mb, d)] ++ alloc x heapleft, otherwise

|| Tests

|| This is the top-level test runner. Evaluate this variable to run all tests.
testall :: bool
testall = emptyheapAlloc6Test &
          emptyheapAlloc16Test &
          emptyheapAlloc0Test &
          usedheapAlloc2Test &
          usedheapAlloc5Test &
          usedheapAlloc6Test &
          garbageheapAlloc0Test &
          garbageheapAlloc2Test &
          garbageheapAllocConsec23Test &
          garbageheapAllocConsec231Test &
          coalesceheapGCTest &
          coalesceheapAlloc1Test  &
          coalesceheapAllocConsec161Test &
          coalescemiddleheapGCTest &
          coalescemiddleheapAlloc4Test &
          coalescemiddleheapAlloc2Test

|| This is an empty heap
emptyheap :: heap
emptyheap = [(0, 16, False, False, [Data 0 | c <- [0..16 - 1]])]

|| This is a used heap, but no garbage blocks, as block with addr 3 and 13
|| are referenced and in-use
usedheap :: heap
usedheap = [(0, 3, False, False, [Data 0 | c <- [0..3 - 1]]),
            (3, 4, True, False,  [Pointer 13, Pointer 13, Pointer 3, Pointer 13]),
            (7, 6, False, False, [Data 0 | c <- [0..6 - 1]]),
            (13, 3, True, False, [Pointer 13, Pointer 3, Pointer 3])]

|| This is a heap with garbage, only block with addr 3 is referenced and used.
|| Block with addr 0, 13 are used but unreferenced (garbage to-be-collected).
garbageheap :: heap
garbageheap = [(0, 3, True, False,   [Pointer 3, Pointer 3, Pointer 3]),
               (3, 10, True, False,  [Pointer 3 | c <- [0..10 - 1]]),
               (13, 3, True, False,  [Pointer 3, Data 0, Data 0])]

|| This is a heap with garbage but involves consecutive coalescing during gc.
|| block with addr 0, 6, 14 will be gc-ed and coalesce into a 15 byte long
|| free block.
coalesceheap :: heap
coalesceheap = [(0, 6, True, False,   [Pointer 15 | c <- [0..6 - 1]]),
                (6, 8, True, False,   [Pointer 15 | c <- [0..8 - 1]]),
                (14, 1, True, False,  [Pointer 15]),
                (15, 1, True, False,  [Data 128])]

|| This is a complex heap with garbage but involves coalescing in the middle
|| of the blocks.
|| Blocks with addr 8, 9, 10, 11 shall be gc-ed and coalesced into a 4 byte
|| long free block.
coalescemiddleheap :: heap
coalescemiddleheap = [(0, 8, True, False,  [Pointer 0 | c <- [0..8 - 1]]),
                      (8, 1, True, False,  [Pointer 0]),
                      (9, 1, True, False,  [Data 200]),
                      (10, 1, True, False, [Data 600]),
                      (11, 1, True, False, [Data 800]),
                      (12, 4, True, False, [Data 100, Data 200, Pointer 12, Data 100])]


|| Test Case 1 - Empty Heap Allocation 6 bytes
emptyheapAlloc6 :: heap
emptyheapAlloc6 = [(0, 6, True, False,   [Data 0, Data 0, Data 0, Data 0, Data 0, Data 0]),
                   (6, 10, False, False, [Data 0 | c <- [0..10 - 1]])]

emptyheapAlloc6Test :: bool
emptyheapAlloc6Test = (malloc 6 emptyheap) == emptyheapAlloc6

|| Test Case 2 - Empty Heap Allocation 16 bytes - full
emptyheapAlloc16 :: heap
emptyheapAlloc16 = [(0, 16, True, False, [Data 0 | c <- [0..16 - 1]])]

emptyheapAlloc16Test :: bool
emptyheapAlloc16Test = (malloc 16 emptyheap) == emptyheapAlloc16

|| Test Case 3 - Empty Heap Allocation 0 bytes
emptyheapAlloc0Test :: bool
emptyheapAlloc0Test = (malloc 0 emptyheap) == emptyheap

|| Test Case 4 - Used Heap Allocation 2 bytes
usedheapAlloc2 :: heap
usedheapAlloc2 = [(0, 2, True, False,   [Data 0, Data 0]),
                  (2, 1, False, False,  [Data 0]),
                  (3, 4, True, False,   [Pointer 13, Pointer 13, Pointer 3, Pointer 13]),
                  (7, 6, False, False,  [Data 0 | c <- [0..6 - 1]]),
                  (13, 3, True, False,  [Pointer 13, Pointer 3, Pointer 3])]

usedheapAlloc2Test :: bool
usedheapAlloc2Test = (malloc 2 usedheap) == usedheapAlloc2

|| Test Case 5 - Used Heap Allocation 5 bytes
usedheapAlloc5 :: heap
usedheapAlloc5 = [(0, 3, False, False,  [Data 0, Data 0, Data 0]),
                  (3, 4, True, False,   [Pointer 13, Pointer 13, Pointer 3, Pointer 13]),
                  (7, 5, True, False,   [Data 0, Data 0, Data 0, Data 0, Data 0]),
                  (12, 1, False, False, [Data 0]),
                  (13, 3, True, False,  [Pointer 13, Pointer 3, Pointer 3])]

usedheapAlloc5Test :: bool
usedheapAlloc5Test = (malloc 5 usedheap) == usedheapAlloc5

|| Test Case 6 - Used Heap Allocation 6 bytes
usedheapAlloc6 :: heap
usedheapAlloc6 = [(0, 3, False, False, [Data 0, Data 0, Data 0]),
                  (3, 4, True, False,  [Pointer 13, Pointer 13, Pointer 3, Pointer 13]),
                  (7, 6, True, False,  [Data 0, Data 0, Data 0, Data 0, Data 0, Data 0]),
                  (13, 3, True, False, [Pointer 13, Pointer 3, Pointer 3])]

usedheapAlloc6Test :: bool
usedheapAlloc6Test = (malloc 6 usedheap) == usedheapAlloc6

|| Test Case 7 - Garbage Heap Allocation 0 bytes (expect garbage collected)
garbageheapAlloc0 :: heap
garbageheapAlloc0 = [(0, 3, False, False,  [Pointer 3, Pointer 3, Pointer 3]),
                     (3, 10, True, False,  [Pointer 3 | c <- [0..10 - 1]]),
                     (13, 3, False, False, [Pointer 3, Data 0, Data 0])]

garbageheapAlloc0Test :: bool
garbageheapAlloc0Test = (malloc 0 garbageheap) == garbageheapAlloc0

|| Test Case 8 - Garbage Heap Allocation 2 bytes
garbageheapAlloc2 :: heap
garbageheapAlloc2 = [(0, 2, True, False,   [Data 0, Data 0]),
                     (2, 1, False, False,  [Pointer 3]),
                     (3, 10, True, False,  [Pointer 3 | c <- [0..10 - 1]]),
                     (13, 3, False, False, [Pointer 3, Data 0, Data 0])]

garbageheapAlloc2Test :: bool
garbageheapAlloc2Test = (malloc 2 garbageheap) == garbageheapAlloc2

|| Test Case 9 - Garbage Heap Consecutive Allocation 3 bytes (following Test 8)
|| First block at address 0 shall be garbage collected and allocted into a 3 byte block
garbageheapAllocConsec23 :: heap
garbageheapAllocConsec23 = [(0, 3, True, False,   [Data 0, Data 0, Data 0]),
                            (3, 10, True, False,  [Pointer 3 | c <- [0..10 - 1]]),
                            (13, 3, False, False, [Pointer 3, Data 0, Data 0])]

garbageheapAllocConsec23Test :: bool
garbageheapAllocConsec23Test = (malloc 3 (malloc 2 garbageheap)) == garbageheapAllocConsec23

|| Test Case 10 - Garbage Heap Consecutive Allocation 1 byte (following Test 9)
|| First block at address 0, again shall be garbage collected and allocated into a 1 byte block
garbageheapAllocConsec231 :: heap
garbageheapAllocConsec231 = [(0, 1, True, False,   [Data 0]),
                             (1, 2, False, False,  [Data 0, Data 0]),
                             (3, 10, True, False,  [Pointer 3 | c <- [0..10 - 1]]),
                             (13, 3, False, False, [Pointer 3, Data 0, Data 0])]
garbageheapAllocConsec231Test :: bool
garbageheapAllocConsec231Test = (malloc 1 (malloc 3 (malloc 2 garbageheap))) == garbageheapAllocConsec231

|| Test Case 11 - Consecutive Coalesce Heap Garbage Collection
coalesceheapGC :: heap
coalesceheapGC = [(0, 15, False, False, [Pointer 15 | c <- [0..15 - 1]]),
                  (15, 1, True, False,  [Data 128])]
coalesceheapGCTest :: bool
coalesceheapGCTest = (malloc 0 coalesceheap) == coalesceheapGC

|| Test Case 12 - Consecutive Coalesce Heap Allocation 1 byte
coalesceheapAlloc1 :: heap
coalesceheapAlloc1 = [(0, 1, True, False,   [Data 0]),
                      (1, 14, False, False, [Pointer 15 | c <- [0..14 - 1]]),
                      (15, 1, True, False,  [Data 128])]
coalesceheapAlloc1Test :: bool
coalesceheapAlloc1Test = (malloc 1 coalesceheap) == coalesceheapAlloc1

|| Test Case 13 - Consecutive Coalesce Heap second round garbage collection
|| The block with addr 15 is not pointed to anymore as the previous blocks are
|| garbage collected.
coalesceheapAllocConsec161 :: heap
coalesceheapAllocConsec161 = [(0, 16, True, False, [Data 0 | c <- [0..16 - 1]])]
coalesceheapAllocConsec161Test :: bool
coalesceheapAllocConsec161Test = malloc 16 (malloc 1 coalesceheap) == coalesceheapAllocConsec161

|| Test Case 14 - Middle Coalesce Heap Garbage Collection
coalescemiddleheapGC :: heap
coalescemiddleheapGC = [(0, 8, True, False,  [Pointer 0 | c <- [0..8 - 1]]),
                        (8, 4, False, False, [Pointer 0, Data 200, Data 600, Data 800]),
                        (12, 4, True, False, [Data 100, Data 200, Pointer 12, Data 100])]
coalescemiddleheapGCTest :: bool
coalescemiddleheapGCTest = (malloc 0 coalescemiddleheap) == coalescemiddleheapGC

|| Test Case 15 - Middle Coalesce Heap Allocation 4 bytes - full
coalescemiddleheapAlloc4 :: heap
coalescemiddleheapAlloc4 = [(0, 8, True, False,  [Pointer 0 | c <- [0..8 - 1]]),
                            (8, 4, True, False,  [Data 0, Data 0, Data 0, Data 0]),
                            (12, 4, True, False, [Data 100, Data 200, Pointer 12, Data 100])]
coalescemiddleheapAlloc4Test :: bool
coalescemiddleheapAlloc4Test = (malloc 4 coalescemiddleheap) == coalescemiddleheapAlloc4

|| Test Case 16 - Middle Coalesce Heap Allocation 2 bytes
coalescemiddleheapAlloc2 :: heap
coalescemiddleheapAlloc2 = [(0, 8, True, False,   [Pointer 0 | c <- [0..8 - 1]]),
                            (8, 2, True, False,   [Data 0, Data 0]),
                            (10, 2, False, False, [Data 600, Data 800]),
                            (12, 4, True, False,  [Data 100, Data 200, Pointer 12, Data 100])]
coalescemiddleheapAlloc2Test :: bool
coalescemiddleheapAlloc2Test = (malloc 2 coalescemiddleheap) == coalescemiddleheapAlloc2
