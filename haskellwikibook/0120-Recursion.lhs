Recursion
--------------------------------------------------------------------------------
2017, Ignacio Matías Sniechowski

Recursion plays a central role in Haskell -and computer science  and mathematics
in general.Recursion is merely a form of repetition;to undersatand it,you should
separate the meaning of a recursive function from its behaviour.  A functions is
recursive when one part of its  definition includes  the function  itself again.
Along with the recursive  condition, these  functions generally  also contain at
least one "base case" condition that stops -i.e.,terminate- the function without
calling the function again. Without a terminating condition, recursive functions
would lead to infinite regress -an infinite loop.

Numeric recursion
--------------------------------------------------------------------------------
The factorial function

Mathematics -specially combinatorics- has a function called factorial -which  is
denoted by n! and means the factorial of a non-negative integer n, but that syn-
tax is impossible in Haskell, so we don't use it. The factorial function takes a
single non-negative integer as argument,  finds all  the positive  integers less
than or equal to n, and multiplies them all together. For example, the factorial
of 6 -denoted in mathematics like 6!- is 6 X 5 X 4 X 3 X 2 X 1 =720.  We can use
a recursive style to define this in Haskell. Let's look at the factorials of two
adjacent numbers:

Factorials of consecutive numbers:

Factorial of 6 = 6 X 5 X 4 X 3 X 2 X 1
Factorial of 5 =     5 X 4 X 3 X 2 X 1

Notice how we have lined things up. You can see here that the 6! include the 5!.
In fact, 6! is just 6 X 5!. Let's continue:

Factorial of 4 =         4 x 3 X 2 X 1
Factorial of 3 =             3 X 2 X 1
Factorial of 2 =                 2 X 1
Factorial of 1 =                     1

The factorial  of any number is  just that number multiplied by the factorial of
the number  less than it. There is one exception: if we ask for the factorial of
0, we don't want to multiply 0 by the factorial of  (-1)  -factorial is  defined
only for positive numbers. In fact, we just  say the factorial of 0 is 1  we de-
fine it to be so, though it is mathematically proven to be so. So, 0 is the base
case for the recursion: when we get to 0 we can immediately  say that the answer
is 1, no recursion needed.We can summarize the definition of the factorial func-
tion as follows:

* 0! = 1
* n! = n X (n-1)!

We can transalte this directly into Haskell:

factorial 0 = 1
factorial n = n * factorial (n - 1)

This defines a new function called factorial.The first line says that the facto-
rial of 0 is 1, and the second line says that the factorial of any  other number
n is equal to n times the factorial of (n - 1). Note the  parentheses around the
(n - 1); without them this would have been parsed as (factorial n) - 1; remember
that function application takes precedence over anything  else when  grouping is
not specified otherwise -we  say that  function application "binds more tightly"
than anything else.

The example above demonstrate the relationship between factorial of a number, n,
and the factorial of a slightly smaller number, (n - 1).Think of a function call
as delegation. The instructions for a recursive function delegate a sub-task. It
just so happens that the delegate function uses the same instructions as the de-
legator; it's only the input data that changes.  The only really confusing thing
about recursive functions is the fact that each function call  uses the same pa-
rameter names, so it can be tricky to keep track of the many delegations.

Let us look at what happens when you execute factorial 3, or 3! in  mathematical
terms:

   factorial 3
=       {applying second clause of factorial definition}
   3 * factorial (3 - 1)
=  3 * factorial 2
=       {applying second clause of factorial definition}
   3 * (2 * factorial (2 - 1))
   3 * (2 * factorial 1)
=       {applying second clause of factorial definition}
   3 * (2 * (1 * factorial (1 - 1)))
   3 * (2 * (1 * factorial 0))
=       {applying first clause of factorial definition}
   3 * (2 * (1 * 1))
=  6

When reading or composing recursive functions, you will rarely  need to "unwind"
the recursion bit by bit as we did above, we leave that to the compiler. One im-
portant note about our recurisve function factorial: the order of the two decla-
rations -one for factorial 0  and one for  factorial n-  is  important . Haskell
decides which function definition to  use by starting at the top and picking the
first one that matches. If we had the general case, factorial n, before the base
case, factorial 0, then the general n would match anything passed into it,inclu-
ding 0. The compiler would then conclude that factorial 0 = 0 * factorial (-1) ,
and so on to negative infinity —clearly not what we want. So, always list multi-
ple function definitions starting with the most specific and  proceeding to  the
most general.

> factorial :: Integer -> Integer
> factorial 0 = 1
> factorial n = n * factorial (n - 1)

* Exercise:  The doubleFactorial  is a function that takes a number n and calcu-
  lates the product of every other number from 1, or 2 up to n. For example:
  doubleFactorial 8 = 8 X 6 X 4 X 2 = 384, and 
  doubleFactorial 7 = 7 X 5 X 3 X 1 = 105. 
  Define the doubleFactorial function.

> doubleFactorial :: Integer -> Integer
> doubleFactorial n
>     | n <= 0       = 1
>     | otherwise    = n * doubleFactorial (n - 2)

Loops, recursion, and accumulating parameters
--------------------------------------------------------------------------------
Imperative languages use loops in the same sorts  of contexts where Haskell pro-
grams use recursion. Depending on the languages you are familiar with, you might
have concerns about performance problems  caused by recursion.  However, Haskell
compilers include a number of optimizations for recursion. Also, Haskell is lazy
-calculations are only performed once their results are required by other calcu-
lations, and that helps to avoid some of the performance overhead.

Other recursive functions
--------------------------------------------------------------------------------
A great number of numeric functions can be defined recursively in a natural way.
For example, let's think about multiplication.  Remember that multiplication can
be thought of as "repeated addition"; that is, 5 X 4 is the same as summing four
copies of the number 5. And of course,summing four copies of the number 5 is the
same as summing three copies,and then adding one more,that is 5 X 4 = 5 X 3 + 5.
This leads us to a natural recursive definition of multiplication.

> mult :: (Eq a, Num a) => a -> a -> a
> mult _ 0   = 0
> mult n 1   = n
> mult n m   = mult n (m - 1) + n

Stepping back, we  can see how numeric recursion fits into the general recursive
pattern.The base case for numeric recursion usually consists of one or more spe-
cific numbers, often 0 or 1, for which the answer can  be immediately given. The
recursive case computes the  result by  calling the function  recursively with a
smaller argument and using the result in some manner to produce a final  answer.
The smaller argument used is often one less than the current argument,leading to
recursion which walks down the number line -like the examples  of factorial  and
mult above. However, the  prototypical pattern is not the  only possibility; the
smaller argument could be produced in some other way as well.

* Exercises: 

1. Define a recursive function power such that "power x y" raises x to the y po-
   wer.
2. You are given a function "plusOne x = x + 1".Without using any other operator
   rather than (+),define a recursive function addition such that "addition x y"
   adds x and y together.
3. Implement the function log2, which computes the integer log (base 2) which is
   less than or equal to its argument. For example, "log2 16 = 4", "log2 11 = 3"
   ,and "log2 1 = 0". 

> power :: (Eq a, Num a) => a -> a -> a
> power 0 _ = 0
> power _ 0 = 1
> power x y = x * power x (y - 1)



> plusOne :: (Eq a, Num a) => a -> a
> plusOne x = x + 1
>
> addition :: (Eq a, Num a) => a -> a -> a
> addition 0 y = y
> addition x 0 = x
> addition x y = plusOne x + addition 0 (y - 1)

List-based recursion
--------------------------------------------------------------------------------
Haskell  has many recursive  functions, especially concerning lists —which is no
coincidence; without mutable variables, recursion is  the only  way to implement
control strucutres.Consider the length function that finds the length of a list:

length :: [a] → Int
length []       = 0
length (x : xs) = 1 + length xs

So,the type signature of length tells us that it takes any type of list and pro-
duces and Int. The next line says that the length of an empty list is 0 -this is
the base case. The final line is the recursive case: if a list isn't empty, then
it can be broken down into a first element -here called x- and the rest  of  the
list  which  will, by convention, be called xs. The length of the list is 1 -ac-
counting for the x- plus the length of xs.

Consider now, the concatenation function (++) which joins two lists together:

(++) :: [a] → [a] → [a]
[] ++ ys       = ys
(x : xs) ++ ys = x : xs ++ ys

This is a little more complicated than length. The type says that (++) takes two
lists of the same type and produces another list of the same type. The base case
says that concatenating the empty list with a list ys is the same as ys  itself.
Finally,the recursive case breaks the first list into its head (x) and tail (xs)
and says that to concatenate  the two  lists, concatenate  the tail of the first
list with the second list, and then tack the head x on the front.

There is a pattern here:with list-based functions,the base case usually involves
an empty list, and the recursive case involves passing  the tail of  the list to
our function again, so that the list becomes progressively smaller.

* Exercises:
Give recursive definitions for the following list-based functions. In each case,
think what the base case would  be, then think what  the general case would look
like, in terms of  everything smaller  than it. Note that all of these functions
are available in the Prelude,so you will want to give them a different name when
testing them.

1. replicate :: Int → a → [a],which takes a count and an element and returns the
   list which is that element repeated that many times: replicate 3 'a' = "aaa".
   Hint: think about what replicate of anything with a count of 0  should  be; a
   count of 0 is your "base case".
2. (!!) :: [a] → Int → a, which returns the element at the given index.The first
   element is at index 0, the second at index 1, and so on.  Note that with this
   function, you are recursing both numerically and down a list.
3. zip :: [a] → [b] → [(a, b)], which  takes two lists and "zips"  them together,
   so that the first pair in the resulting list is  the first two elements of the
   two lists, and so on. For example: 
                              zip [1, 2, 3]  "abc" = [(1, 'a'), (2,'b'), (3,'c')]
   If either of the lists is shorter than the other,you can stop once either list
   runs out; i.e., zip [1,2]  "abc" = [(1,'a'),(2,'b')].
4. Define length using an auxiliary function and an accumulating parameter, as in
   the loop-like alternate version of factorial.

> replicate' :: Int -> a -> [a]
> replicate' 0 x = []
> replicate' n x = replicate' (n - 1) x ++ [x]


> (!-!) :: [a] -> Int -> a
> (!-!) [] n     = error "empty list"
> (!-!) (x:xs) 0 = x
> (!-!) (x:xs) n = undefined

Recursion is used to  define  nearly all functions to do with lists and numbers.
The next time you need a list-based algorithm, start  with a case for the  empty
list and a case for the non-empty list and see if your algorithm is recursive.

A final note on recursion
--------------------------------------------------------------------------------
Despite its ubiquity  in Haskell, one rarely has to write functions that are ex-
plicitly recursive. Instead, standard library functions perform recursion for us
in various ways. For example, a simpler way to implement the factorial  function
is:       
                                                    factorial n = product [1..n]

Although it might seem like cheating, this is the version of factorial that most
experienced Haskell programmer would write, rather than the explicitly recursive
version. 

