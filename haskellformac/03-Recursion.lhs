Recursion
--------------------------------------------------------------------------------
The functions that  we considered so far only used a  fixed number of elementary
operations. Even

> module Recursion where
> import Data.Char
>
> type Colour = String
> type ColourPoint = (Int, Int, Colour)
>
> distance :: ColourPoint -> ColourPoint -> Float
> distance (x1, y1, colour1) (x2, y2, colour2)
>   = sqrt (fromIntegral (dx * dx + dy * dy))
>   where
>    dx = x2 - x1
>    dy = y2 - y1

Needs exactly one addition, two subtractions, two  multiplications, the  invoca-
tion of  fromIntegral, and one  square root &mdash;which makes seven operations.
If conditional expressions or guards are used,the number of operations may vary,
but we can still place an upper limit on the number of operations (independently
of the input passed to the function). However, many problems cannot be solved by
functions  limited in this  way; indeed, some functions -depending on the input-
have to perform  an arbitrarily large number of operations. In the following, we
look  at a programming technique  known as *recursion* as the fundamental mecha-
nism to implementing such functions in Haskell.

Recursion over Numbers
--------------------------------------------------------------------------------
Consider the  function natSum, which computes  the sum of all natural numbers up
to  a limit, or the  Prelude function product, which computes the product of all
the elements of an integer list:

natSum  n                  => n + (n - 1) + ... + 2 + 1 + 0
product [x1, x2, ... , xn] => x1 * x2 * ... * xn

The above are not actual function definitions, since the notation ... is not va-
lid Haskell. However, they illustrate the meaning of the two functions in reaso-
nably formal terms.From this specification of the meaning,we see that both func-
tions require n operations to compute their result. Thus, we can make two impor-
tant observations:

1. The number of operations depends on the input.
2. A certain computation  (or more generally, a set of operations) is repeatedly
   used.

It is this input-dependent repetition that we will implement by recursion.

Computing n + ... + 2 + 1 + 0
--------------------------------------------------------------------------------
Let us start with the simplest case: recursion over the natural numbers. How can
we define the function  "natSum :: Num a => a -> a",  which sums  up all natural
numbers from zero up to a given number n? It should behave as:
                                                 natSum n = n + ... + 2 + 1 +  0
but  how can we substitute  the ellipsis by working program code? To get an idea
of what  we would like to  happen, consider the  following rules  describing the
computations to be performed by natSum for input values up to 5:

natSum 0 =                     0
natSum 1 =                 1 + 0
natSum 2 =             2 + 1 + 0
natSum 3 =         3 + 2 + 1 + 0
natSum 4 =     4 + 3 + 2 + 1 + 0
natSum 5 = 5 + 4 + 3 + 2 + 1 + 0
...

The above are  legal Haskell definitions, but oviously, we would need an unboun-
ded  number of definitions  to define natSum for every possible input. There is,
however, an  observation that  comes to our rescue: each  of the above equations
contains computations that already appear in the previous equations.For example,
for natSum 5, we  have to evaluate 0 + 1 + 2 + 3 + 4 + 5, but the subcomputation
0 + 1 + 2 + 3 + 4 already appears in natsum 4. This seems like good news,  as it
allows us to  reuse earlier equations  for later ones. In  other words, we could
redefine natSum 5 as:

natSum 5 = 5 + natSum 4

In fact, except for the first equation, we can systematically reuse the immedia-
tely preceding equation:

natSum 0 = 0
natSum 1 = 1 + natSum 0
natSum 2 = 2 + natSum 1
natSum 3 = 3 + natSum 2
natSum 4 = 4 + natSum 3
natSum 5 = 5 + natSum 4
...

Interestingly, all equations -except for the first one- now look exactly the sa-
me; they just use  different values. This seems like and ideal situation to once
more apply the trick that we used when we introduced function definitions, i.e.,
we use  abstraction to replace concrete values by variables and, in this way, we
distill a repeating pattern out of the above equations.The repeating pattern is:

natSum 0 = 0
natSum n = n + natSum (n - 1)

We seem to have capture the essence of natSum. In natural language,we could des-
cribe this essence as follows:  "The sum of the natural numbers from 0 to 0 is 0
(first case). The sum of the natural numbers from 0 to n can be obtained by com-
puting the sum of the natural  numbers form 0 to (n - 1) and then, adding n (se-
cond case)."

This  sounds sensible:  indeed, it is sufficient to compute the result of natSum
for every case.For example, let us look at the stepwise evaluation of one appli-
cation of natSum:

natSum 5 => 5 + natSum (5 -1)
         => 5 + natSum 4
         => 5 + (4 + natSum (4 - 1))
         => 5 + (4 + natSum 3)
         => 5 + (4 + (3 + natSum (3 - 1)))
         => 5 + (4 + (3 + natSum 2))
         => 5 + (4 + (3 + (2 + natSum (2 - 1))))
         => 5 + (4 + (3 + (2 + natSum 1)))
         => 5 + (4 + (3 + (2 + (1 + natSum (1 - 1)))))
         => 5 + (4 + (3 + (2 + (1 + natSum 0))))
         => 5 + (4 + (3 + (2 + (1 + 0))))
         => 5 +  4 +  3 +  2 +  1
         => 15

This  obviously works  the way we intended  it to work. The above  definition of
natSum is called "recursive", because natSum itself is used in the definition of
natSum -i.e., a recursive function is a function that makes use of itself in its
definition.
Recursive function definitions have at least two cases: the "base case"  and the
"recurisve case" (or "stepping case"). The base case specifies what to do in the
simplest form  of input (where the function  stops calling  itself), whereas the
stepping case includes the recursive use of the function by itself:

> natSum :: (Eq a, Num a) => a -> a
> natSum n = if n == 0
>              then 0
>              else n + natSum (n - 1)

It contains only  one equation and makes the case distinction explicit through a
conditional. A  question that  might come up during the definition of natSum is,
what  happens if we call this  function with an arguments that causes the recur-
sive case to move away from, rather than towards the base case.For example, what
does "natSum (-1)"  result in? As the recursive case is applicable, the argument
will be reduced to -2, -3, and so on, which means that we enter an infinite com-
putation. In other words, natSum is not defined for arguments smaller than 0.

This is another instance of the concept of partial functions, which we discussed
in  earlier chapters. However, here the problem is not simply a lack of an unde-
fined input pattern,but that the function fails to terminate for some of the in-
put values admitted by its  type signature. To replace non-termination by a pro-
per runtime error, we can use the previously discussed "error" function:

> natSum' :: (Num a, Ord a) => a -> a
> natSum' 0        = 0
> natSum' n
>    | n > 0      = n + natSum' (n - 1)
>    | otherwise  = error "natSum: Input value less than zero"

Recursion and Lists
--------------------------------------------------------------------------------

List construction
We  can not only use recursion  to calculate numbers, but also to build lists: A
simple  example of such  a recursive function  is repeatN, which produces a list
that contains a given item n times; i.e., has type "repeatN :: Int -> a -> [a]".
To find a suitable definition for the function, we first consider what an appro-
priate base case  might look like.  Let us assume that we  want the  function to
work for  zero repetitions. Then, the expression "repeatN 0 x" would have to re-
turn an empty list, which fully specifies the base case.

Next,  we have to find a suitable recursive case. If we already had a definition
that  works  for   "repeatN (n - 1) x",  how  would  we  extend  the  result  of
"repeatN (n - 1) x" such that it gives us the result for "repeatN n x"?  Another
way to  ask the same question  is, which operation is it that should be repeated
in the recursive definition? The  answer is using the colon operator to add ano-
ther copy of the replicated item to the result string. So, overall, we get

> repeatN :: Int -> a -> [a]
> repeatN 0 x = []
> repeatN n x = x : repeatN (n - 1) x

Given  the material that we  covered so far, it should be relatively strightfor-
ward to write  a function that, when  given a string, produces a list containing
all of the possible suffixes of that string. For example, we would have:

suffixes "Hello"  => ["Hello", "ello", "llo", "lo", "o"]

The  base case is when  we have an empty string; then,  we have no suffix, so we
return the empty list:

suffixes "" = []

On the other hand, given:

suffixes "ello"  => ["ello", "llo", "lo", "o"]

We only need to add the string "Hello" at the front of the result to get the va-
lue of "suffixes "Hello". Moreover, as:

tail "Hello"  => "ello"

we arrive at the following definition:

> suffixes :: String -> [String]
> suffixes ""  = []
> suffixes str = str : suffixes (tail str)

In other word, after adding the current string str, we only need the suffixes of
"tail str".Note that we can build lists recursively using only the empty list []
and the list forming operator (:). As these two suffice to build any  list, they
are regarded  as the basic list  forming constructors. In fact, they are usually
called "nil" and "cons" respectively.  (The word 'nil'  actually stands for "Not
In List" and 'cons' is an abbreviation for "(list) constructor")

Lists as recursive structures
--------------------------------------------------------------------------------
All lists are constructed from nil and cons, where the following equality illus-
trates the correspondence between the square bracket and the nil/cons notation:

[x1, x2, ... , xn] = (x1 : (x2 : ... (xn : []) ... )

Due to the repeated occurrence of cons,the right hand side exposes the recursive
structure of lists. For each element xi in a list, we have one cons operator in-
cluding this element into the list. Finally, each list is terminated by nil.This
representation not only makes the recursive nature of lists explicit, but it is,
in fact, the original representation of lists. The closed [x1, x2, … , xn] nota-
tion is only convenient shorthand.

Pattern matching on lists
--------------------------------------------------------------------------------
The  nil and cons operators are so elementary that we not only use them to cons-
truct lists, but also to decompose them; much as we did with pattern matching to
decompose tuples in this definition of fst:

fst :: (a, b) -> a
fst (x, y) = x

In a similar manner, we use pattern matching to decompose lists into their first
element and the rest of  the list; i.e, into  the two components joined together
by cons. In fact, this is exactly what the two functions head and tail do to ex-
tract the first element and the remaining elements from a list:

head :: [a] -> a
head (x:xs) = x

tail :: [a] -> [a]
tail (x:xs) = xs

In other  words, they yield the two values that are used to compose a list using
the cons operator. Thus, for every non-empty list xs, we have the equality:

xs = head xs : tail xs

Therefore, the  first component  passed to cons is often  called the head of the
new list and the  second component  the tail. In previous chapters, we discussed
that  head and  tail are partial  functions as they  lack a pattern matching the
empty list.  If we want to define a total function  over lists with pattern mat-
ching, we  have to specify at  least two cases, one for the case where the input
is  an empty list and  a second for the case where it is not empty; i.e., can be
regarded  as being constructed  by a cons operator. The following function (also
included  in the  Prelude), which  checks   whether a given list is empty, cover
both cases:

null :: [a] -> Bool
null []      = True
nul (x:xs)   = False

Mapping: applying an operation to every element of a list
--------------------------------------------------------------------------------
Combining pattern matching with recursion,we can traverse a list from the begin-
ning to end. Let's say we have a list of numerals and want to compute the square
of each element and return the resulting squared numbers as a new list:

allSquared [x1, x2, ..., xn] = [x1 * x1, x2 * x2, ..., xn * xn]

For  the base case, that is,  empty list, we just  return the empty list. If the
list consists of a head x and a tail xs (pronounced: xes, as in boxes), we build
a new list, with x*x as head, and the result of the recursive call allSquared xs
as tail:

> allSquared :: Num a => [a] -> [a]
> allSquared []      = []
> allSquared (x:xs)  = x * x : allSquared xs

With the same list traversal pattern, we can define a function allToUpper which
converts a string to upper case.

allToUpper "can you hear me now?"  => "CAN YOU HEAR ME NOW?"

To do so, we use a function defined in the standard module Data.Char called:
                                                       "toUpper :: Char -> Char"
Which converts  a lower case  letter to an uppercase letter and leaves all other
characters as they are:

> -- the "import Data.Char" at the beginning is for this function
> allToUpper :: String -> String
> allToUpper ""                 = ""
> allToUpper (chr : restString) = toUpper chr : allToUpper restString 

Apart from the names of the  functions and variables, and that we have to import
the module Data.Char, the functions allSquared and allToUpper are almost identi-
cal —both follow the pattern:

recursiveFunction []       = []
recursiveFunction (x : xs) = doSomethingWith x : recursiveFunction xs

Such  functions can get additional arguments than the list as parameter. For ex-
ample,re-using the definition of ColourPoint from the beginning, we might define
a function that, given a "point :: ColourPoint" together  with a list  of points
"points :: [ColourPoint]", calculates the distance of each  point in "points" to
"point":

> distancesFromPoint :: ColourPoint -> [ColourPoint] -> [Float]
> distancesFromPoint pt []     = []
> distancesFromPoint pt (p:ps) = distance pt p : distancesFromPoint pt ps 

This  function still follows the same pattern  of recursive list traversal as do
allSquared and allToUpper.

Filtering: removing elements from a list
--------------------------------------------------------------------------------
The two functions allSquared and allToUpper produced lists with exactly the same
length as their input list.Let's continue with a string example: given a string,
how can we extract all the digits.
For example:  "extractDigits "das43 dffe 23 5 def45" should  return  the  string
"4323545"`,using the function: "isDigit :: Char -> Bool" from Data.Char to check
whether a single character is a digit.

The base case is (as almost always), straightforward: if we get an empty string,
we just return the empty string. The recursive case is slightly more complicated
than in previous examples: we have to check if the first character is a digit in
which  case we include it in the result.  Otherwise, we  ignore it and just call
"extractDigits" on the tail of the input list:

> extractDigits :: String -> String
> extractDigits []                 = []
> extractDigits (chr : restString)
>     | isDigit chr = chr : extractDigits restString
>     | otherwise   =       extractDigits restString

While the structure of this  function definition is still quite close to that of
allSquared and allToUpper, the  most obvious  difference is the use of guards to
distinguish between two different recursive cases: one that uses "chr",the first
element of the input list, and one that disregards that element.

Another example, using the "ColourPoint" type again, receives a  point and a ra-
dius in addition to  a list of  points as arguments, and  then, yields a list of
points which  are located within the radius around the point passed as the first
argument:

> inRadius :: ColourPoint -> Float -> [ColourPoint] -> [ColourPoint]
> inRadius point radius []       = []
> inRadius point radius (p : ps)
>    | distance point p <= radius  = p : inRadius point radius ps
>    | otherwise                   =     inRadius point radius ps

Reductions: combining the elements of a list
--------------------------------------------------------------------------------
Another important pattern of recursive functions are "reductions", which combine
the elements of a collection, such as a list, into a new value.An example is the
"product" function, which we mentioned  at the start of  this chapter  and which
computes the product of all the elements of a given list:

product [x1, x2, ..., xn] => x1 * x2 * ... * xn

Using our knowledge about the recursive nature of lists,we can rephrase the task
as defining a function product computing

product (x1 : (x2 : ... : (xn : []) ... ) = x1 * x2 * ... * xn

By fixing a suitable order for the calculation of the products on the right hand
side and adding a neutral value of multiplication (i.e. 1),we get the right hand
side exactly into the same format as the recursive list representation,replacing
":" with "*", and "[]" with the neutral element of multiplication, 1:

product (x1 : (x2 : ... : (xn : []) ... ) = (x1 * (x2 * ... * (xn * 1) ... )

Thus, we can use a  recursive function  definition following the recursion inhe-
rent in the list data structure -this is known as "structural recursion".For the
base case, we have "product" applied to an empty list, i.e., nil. From the above
specification of "product", it is clear that we have to return 1(the neutral va-
lue) in this case:

product [] = 1

The recursive case is that of a non-empty list,where cons allows us to decompose
the list  into a head and a tail. We multiply the head with the result of recur-
sively applying "product" to the tail of the list. So, overall, we have

> product' :: Num a => [a] -> a
> product' []      = 1
> product' (x:xs)  = x * product' xs

This definition  clearly does what we said before. It replaces nil by 1 and cons
by multiplication. Reductions like product constitute a common pattern of struc-
turally recursive functions.

* Inside reductions: To  see how a list  traversal  proceeds, let us look at the
  stepwise evaluation of "product [3, 5, 6]", which we write as:
                                                     "product (3 :(5 :(6 :[])))"
  to make the recursive structure of the list explicit:

  product (3 : (5 : (6 : [])))   => 3 * product (5: (6 :[]))
                                 => 3 * (5 * product (6 : []))
                                 => 3 * (5 * (6 * product []))
                                 => 3 * (5 * (6 * 1))
                                 => 90

* A general recursive  pattern of reductions:  There are many  other examples of
  commonly  used reductions. For instance,  calculating the sum of a list is al-
  most identical to the "product" function:

> sum' :: Num a => [a] -> a
> sum' []     = 0
> sum' (x:xs) = x + sum' xs

  Calculating the minimum or maximum of a list is also similar. However, in this
  case, it is not clear what the return value for an empty list ought to be. One
  option  is to leave the  case of empty lists  undefined and proceed as follows
  (note that we're using `min` as infix operator by enclosing it in backquotes).

> minList :: [Int] -> Int
> minList (x:[])  = x
> minList (x:xs)  = x `min` minList xs

  The  first equation  applies  whenever the  argument list has exactly one ele-
  ment. The second, by itself, applies whenever a list has at least one element;
  sp, the two patterns overlap. However, for any given  argument,code evaluation
  tests patterns starting with the first equation and then working its way down.
  The first equation with a matching pattern will be selected.  Hence, if a list
  has exactly one element, minList will always pick the first equation;  it will
  select the second equation if the argument list has more than one element.  If
   he list is empty, the  result is undefined; i.e.,this version of minList is a
  partial function.

  Alternatively, we might consider the largest possible value of Int type a neu-
  tral value of the min function and use it to define the base case. In Haskell,
  the range of Int is implementation dependent; hence, we use the Prelude value
  maxBound, which yields the largest Int value. 
  Stricktly speaking, "maxBound :: Bounded a => a" is a member of the type class
  Bounded and works on a range of types, not just Int:

> minList' :: [Int] -> Int
> minList' []     = maxBound
> minList' (x:xs) = x `min` minList' xs

  Looking at these examples,the general pattern for list reductions takes a list
  of the form:

  (x1 : (x2 : ( ... : (xn : []) ... )

  and replaces the cons (:) and nil ([]) operators with a binary function op and
  some starting value n, respectively, to yield:

  (x1 `op` (x2 `op` ... `op` (xn `op` [] ... )

* More examples of reductions:  The  result of a  reduction may  even be another
  list:  in fact, the list append operator "(++) :: [a] -> [a] -> [a]", which we
  used in our first chapter, is an instance of such a function. This becomes ob-
  vious when we denote the behaviour of (++) as follows:

  (x1 : (x2 : ... : (xn : []) ... ) ++ ys  => (x1 : (x2 : ... : (xn : ys) ... )

  Here, we replace  cons (:) by itself  (i.e., it is the binary operator of this
  reduction) and we replace nil ([]) by the appended list ys. With that insight,
  the definition of  (++) is straightforward  (using infix notation  for (++) in
  the definition of the two equations):

  (++) :: [a] -> [a] -> [a]
  []     ++ ys  = ys
  (x:xs) ++ ys  = x : (xs ++ ys)

  Given this definition, let us see how it evaluates:

  (5:(6:(2:[]))) ++ (4:(2:[])) => 5:((6:(2:[])) ++ (4:(2:[])))
                               => 5:(6:((2:[])  ++ (4:(2 []))))
                               => 5:(6:(2:([]   ++ (4:(2: [])))))
                               => 5:(6:(2:(4:(2:[]))))
                               => 5:6:2:4:2:[]
                               => [5,6,2,4,2]


  Lists  in Haskell can  be nested, i.e., we  can have lists of lists of integer
  numbers, such as:
                                                       [ [5, 6, 2], [] ,[4, 2] ]

  A commonly used Prelude function that takes a list  of lists and reduces it to
  a list with one nesting level fewer is concat -this is an example:

  concat [[5, 6, 2], [], [4, 2]]  => [5, 6, 2, 4, 2]

  As we can join two lists with the ++ operator,we can write a specification for
  concat as follows:

  concat [x1, x2, ..., xn]  => x1 ++ x2 ++ ... ++ xn

  This translates directly into the recursive  pattern of  reductions, replacing
  (:) by (++) and [] by itself:

  concat (x1:(x2:...:(xn:[])...) => (x1 ++ (x2 ++...++ (xn ++ [])...)

  We rewrite this specification into Haskell as before, to produce:

> concat' :: [[a]] -> [a]
> concat' []       = []
> concat' (xs:xss) = xs ++ concat' xss

  Finally, let  us look at a slightly trickier function.  The function  reverse.
  "reverse :: [a] -> [a]" reverses the order of elements in a list:

  reverse "Hello"  => "olleH"

  Interestingly,  we can  express reverse  as a reduction as well. This may seem
  counterintuitive at first, considering the general specification of reverse:

  reverse [x1, x2, ..., xn]  => [xn, ..., x2, x1]

  which gives us a recursive pattern that doesn't seem to fit a reduction:

  reverse (x1:(x2:...:(xn:[])...) => (xn: ...:(x2:(x1:[])...)

  Here, we don't replace (:) and [], but instead we replace the elements. Let us
  take a step back. So far, we have always  replaced cons (:) by  some  existing
  standard function (such as (+) or min). How  about replacing it  by a function
  of  our own design instead? In other words, to implement reverse using the re-
  duction  pattern, we are looking for a function "snoc" that we can use as fol-
  lows:

  reverse (xn:...:(x2:(x1:[])...) =>
                               (x1 `snoc` (x2 `snoc`...`snoc` (xn `snoc` [])...)

  The function `snoc` needs to have the same type signature as: 
                          (:) :: a -> [a] -> [a]  <=>   `snoc :: a -> [a] -> [a]

  Considering the first invocation of `snoc` in the call: reverse "Hello" (which
  is the same as reverse ('H' : 'e' : 'l' : 'l' : 'o' : []),we know that we need

  'H' `snoc` "olle"  => "olleH"

  The cons operator (:) can  only  attach  elements  at the  head  of a list and
  `snoc` is essentially the reverse of that (hence, the name).  To attach at the
  end, we need to use (++) -i.e., we have got

> snoc :: a -> [a] -> [a]
> snoc x xs = xs ++ [x]


  And following the familiar pattern for reduction

> reverse' :: [a] -> [a]
> reverse' []     = []
> reverse' (x:xs) = x `snoc` reverse' xs


  Given the  simplicity  of `snoc`, we  may  like to inline its  definition into
  reverse, which gets us:

> reverse'' :: [a] -> [a]
> reverse'' []     = []
> reverse'' (x:xs) = reverse'' xs ++ [x]

  This approach of using our own function to replace (:) by way of the recursive
  pattern for reductions is a very powerful technique. In fact, any structurally
  recursive function can be implemented like that.  This does not always lead to
  an intuitive definition of a given function, but it is certainly possible.

  Getting back to our definition of reverse, the definition  works, but it has a
  serious problem: from the definition of (++), we know that to  append a single
  element [x] at the end of the list, we have to recurse down the whole list un-
  til we reach [], which we then replace by [x]. Reverse has to do this in every
  single recursive step! That is quite computationally expensive.

Left-associative reductions
--------------------------------------------------------------------------------
The recurisve pattern that we discussed so far replaces cons (:) and nil ([]) in

(x1 :(x2 : ... : (xn : []) ... )

With a binary  function 'op'  and some  starting value n starting from the right
and proceeding to the left:

(x1 `op` (x2 `op` ... `op` (xn `op` n) ... )

In other words, we start by computing (xn `op` []). Then, we  use that result to
compute  (xn-1 `op` (xn `op` [] )), and  so forth. This is  often called a "righ
associative reduction" or "reduction from the right". That is often what we want
,and for "associative operators", such as  addition and multiplication, where we
have:

a + (b + c) = (a + b) + c

The direction of the reduction is irrelevant.  In other cases, we could actually
like to proceed from the left to the right and compute

( ... (n `op` x`) `op` x2) `op` ... `op` xn)

This, in particular, implies that we combine the starting value n  from the left
with x1; instead of combining it from the right with xn. As an example, consider
the function:

deductFromAccount :: Int -> [Int] -> Int

Which gets the initial balance of an account  in cents  together  with a list of
deductions from that account in  the order in which they are to be performed. On
that basis, it needs to calculate  the resulting account  balance  with the fol-
lowing exception: if a deduction would result in a negative account balance, the
function should return an error message:

deductFromAccount 75645 [322,434,5343,234]
                                     => (((75645-322)-434)-5343)-234
deductFromAccount 75645 [322,80000,5343,234]
                                     => error "Your account balance is negative"

To implement this function, we need to implement a reduction proceeding from the
left to rigth over the input list, where the initial balance is the starting va-
lue n from our pattern above. We need to compute:

deductFromAccount balance (x1 : (x2 : ... : (xn : []) ... )
           => ( ... (balance `deduct` x1) `deduct` x2) `deduct` ... `deduct` xn)

For a suitable definition of `deduct`. It needs to use a conditional or guard to
check whether the current balance is larger than the next deduction:

balance `deduct` x
    | balance < x = error "Your account balance is negative"
    | otherwise   = balance - x

The general scheme to implement the pattern for left reduction is

leftReduce n []     = []
leftReduce n (x:xs) = leftReduce (n `op` x) xs

By substituting balance for n and `deduct` for `op`, and then, inlining `deduct`
,we get the complete definition for deductFromAccoun:

> deductFromAccount :: Int -> [Int] -> Int
> deductFromAccount balance []    = balance
> deductFromAccount balance (d:ds)
>    | balance < d  = error "Your account balance is negative"
>    | otherwise    = deductFromAccount (balance - d) ds

The parameter balance acts as an accumulator, passing  down  information of what
happened earlier in the list.

deductFromAccount 75645 [322, 434, 5343, 234]
                  => deductFromAccount (75645 - 322) [434, 5343, 234]
                  => deductFromAccount ((75645 - 322) - 434) [5343, 234]
                  => deductFromAccount (((75645 - 322) - 434) - 5343) [234]
                  => deductFromAccount ((((75645 - 322) - 434) - 5343) - 234) []
                  => ((((75645 - 322) - 434) - 5343) - 234)
                  => 69312

In contrast to the right-associative recursive pattern, where  the work  (adding
the numbers, computing the minimum, etc) happens  after the  recursive  function
call returns a results,  the work here is in the computation of the accumulator,
which is simply being returned by the base case.

* A second example of a left-associative reduction:  Another example that requi-
  res a reduction from the left is the function  "stringToInt :: String -> Int",
  which converts a string of digits into the corresponding Int value:

  stringToInt "43212"  => 43212

  We can use the function "digitToInt :: Char -> Int" from module "Data.Char" to
  convert a single character, but the calculation for a string is more complica-
  ted:

  stringToInt "43212" => 10000 * 4 + 1000 * 3 + 100 * 2 + 10 * 1 + 2 * 1

  To expose the recursive pattern, we rewrite this to

  10*(10*(10*(10 * digitToInt '4' + digitToInt '3') + digitToInt '2') +
                                              digitToInt * '1') + digitToInt '2'

  As with balance, we use an accumulator argument to pass the result of the pre-
  ceding calculations on to the recursive calls. In each recursive step, we have
  to multiply the accumulator by ten and add the digit value of the current cha-
  racter. To this end, we define a helper function "stringToIntAcc" that  imple-
  ments the left-reduction using an additional argument as the accumulator. This
  time around, we omit the  individual steps of instantiating the left-reduction
  pattern and implementing  the application  specific version of the combination
  function op; instead we  immediately proceed to the final recursive definition
  and leave the skipped steps as an exercise.

> stringToInt :: String -> Int
> stringToInt str = stringToIntAcc 0 str
>
> stringToIntAcc :: Int -> String -> Int
> stringToIntAcc acc []            = acc
> stringToIntAcc acc (c:rSt) = stringToIntAcc (10 * acc + digitToInt c) rSt

  Since the  function  stringToIntAcc is only a  helper function  and not called
  from anywhere else but stringToInt, we would  like to define  it as a function
  that is only visible inside the body of stringToInt; so that stringToIntAcc is
  local to stringToInt.  We can do this in the same manner  as for variable bin-
  ding using a where clause:

> stringToInt' :: String -> Int
> stringToInt' str = stringToIntAcc 0 str
>  where
>   stringToIntAcc :: Int -> String -> Int
>   stringToIntAcc acc []      = acc
>   stringToIntAcc acc (c:rSt) = stringToIntAcc (10 * acc + digitToInt c) rSt

  Defining stringToIntAcc locally is not necessary, but it is good style.

* Revisiting reverse:. When we implemented the reverse function with a right-to-
  left reduction, we observed at the end that the code is quite inefficient. The
  recursion for reverse visits each element  of the input  list  once, and then,
  calls (++) once for each  element, which is  itself a recursive  function that
  visits each element of its first argument.  Overall, this  implies that  for a
  list of size n, our implementation of reverse  performs  in the  order of  n^2
  function calls. This is excessive.  We would expect  to be able  to do it with
  about n function calls.

  The reason for the use of (++), and hence the  inefficiency, in our definition
  of reverse was to add a single  element x to a  list xs  using  the expression
  xs ++ [x]. This was necessary as we used  to  right-to-left reduction pattern.
  This raises the question of whether we can do better with left-to-right reduc-
  tion. Again, we leave  writing out the  left-associative reduction pattern for
  reverse as an exercise and go straight to the final implementation.

  The intuition for the implementation of fastReverse is  that  the  accumulator
  argument of the left-to-right reduction carries the already reversed prefix of
  the list down the recursive calls.As (:) is left-associative and we start with
  the  leftmost element of the input list, we can add it with (:) to the accumu-
  lator to end up with the  entire reversed  list when the  recursion encounters
  the base case.

> fastReverse :: [a] -> [a]
> fastReverse xs  = reverseAcc [] xs
>  where
>    reverseAcc :: [a] -> [a] -> [a]
>    reverseAcc accList []     = accList
>    reverseAcc accList (x:xs) = reverseAcc (x : accList) xs

  As there is a constant number of operations per  recursive call  of reverseAcc
  ,this new definition meets our  performance goal.  This becomes  obvious  when
   using stepwise evaluation on an example invocation:

  fastReverse "321"
                    => reverseAcc [] "321"
                    => reverseAcc ('3' : []) "21"
                    => reverseAcc ('2' : '3' : []) "1"
                    => reverseAcc ('1' : '2' : '3' : []) ""
                    => ('1' : '2' : '3' : [])
                    => "123"

* Associative binary operations: As mentioned earlir, we can  use both right-to-
  left of left-to-right  reduction in the case where the binary operation of the
  reduction is associative and the starting value is a neutral value for the bi-
  nary operation. As an example, consider the following definition of sum, which
  sums up the elements of a list (and is almost the same as product,  but  using
  addition instead of multiplication).

> suma :: Num a => [a] -> a
> suma xs = sumAcc 0 xs
>   where
>    sumAcc :: Num a => a -> [a] -> a
>    sumAcc acc []     = acc
>    sumAcc acc (x:xs) = sumAcc (x + acc) xs

  Due to certain optimisations expected  of a Haskell  compiler, this  version of
  sum is in fact more efficient that he version using the right-to-left reduction
  pattern, as it is able to execute using a constant amount of call stack memory.
  These operational subtleties are, however, and advanced topic that we will save
  for later.

  Reductions are a very powerful recursive pattern.  We will aslo revisit them in
  the context of more advanced chapters.

Combining different patterns
---------------------------------------------------------------------------------
We discussed four recursive patterns over lists: mapping, filtering,and two forms
of reductions.Some problems require a combination of these patterns. Two examples
are:

1. to calculate the sum of all even elements of a list
2. to calculate the sum of the square roots of all positive numbers in a list

We can solve the first problem -i.e., computing  the sum of all even elements- by
first filtering out all even elements, and  then, calculating the sum of these e-
ven elements as follows:

> -- to keep things shorter, we use the 'sum' function defined in the 'Prelude
> sumEvenElems :: (Integral a, Num a) => [a] -> a
> sumEvenElems xs = sum (filterEven xs)
>  where
>    filterEven []     = []
>    filterEven (x:xs)
>      | even x     = x : filterEven xs
>      | otherwise  = filterEven xs

Alternatively, we can pick out the even  elements and  compute the sum in one re-
cursive traversal:

> sumEvenElems' :: (Integral a, Num a) => [a] -> a
> sumEvenElems' [] = 0
> sumEvenElems' (x:xs)
>  | even x     = x + sumEvenElems' xs
>  | otherwise  = sumEvenElems' xs

Similarly, we can on the one hand, implement the sum of square roots of all posi-
tive numbers as the application of three separate functions:

> sumOfSquareRoots :: (Ord a, Floating a) => [a] -> a
> sumOfSquareRoots xs = sum (allSquareRoots (filterPositives xs))
>   where
>     allSquareRoots []     = []
>     allSquareRoots (x:xs) = sqrt x : allSquareRoots xs
>    
>     filterPositives []   = []
>     filterPositives (x:xs)
>       | x > 0       = x : filterPositives xs
>       | otherwise   = filterPositives xs

On the other hand, we can combine all three recursive traversals into one:

> sumOfSquareRoots' :: (Ord a, Floating a) => [a] -> a
> sumOfSquareRoots' []     = 0
> sumOfSquareRoots' (x:xs)
>   | x > 0      = sqrt x + sumOfSquareRoots' xs
>   | otherwise  = sumOfSquareRoots' xs

Which is the better style? Generally, the first version using separate traversals
is considered better style, as it is often more readable and, especially, because
there is a set of powerful Prelude functions  (which we will  discuss in a  later
chapter) that we can actually use instead of writing the individual (quite simple
functions) ourselves. Hence, we get better code reuse.

We might be concerned that the easier to read version is less  efficient. However
,due to Haskell's so-called, lazy evaluation execution  strategy, the overhead is
significantly less than in  most other  programming languages.  Moreover,  a good
Haskell compiler can usually  derive the one traversal version from the more ele-
gant multiple traversal version using a process called fusion. In other word, un-
less performance profiling of a program shows that you need to hand-code the more
efficient version, you should always favour readability.

A More Complex Example
---------------------------------------------------------------------------------
Let us consider the function "closestPoint :: Point -> [Point] -> Point",  which,
given a point p and  a list  of points  [points], returns  the point in  [points]
which is located closest to p. If there is more than  one such point, the one ap-
pearing first is returned). The type Point is just a synonym for a pair  of floa-
ting point values:

> type Point = (Float, Float)

See function "distance", at the  beginning of this  chapter, on  how to calculate
the distance between two points.

> points :: [Point]
> points = [(1,1), (2, 2), (6, 5), (-3, 4), (3, 4)]
>
> distance' :: Point -> Point -> Float
> distance' (x1, y1) (x2, y2)
>   = sqrt (dx * dx + dy * dy)
>    where
>     dx = x2 - x1
>     dy = y2 - y1
>    
> closestPoint :: Point -> [Point] -> Point
> closestPoint point [p]    = p
> closestPoint point (p:ps) = closestOfTwo point p (closestPoint point ps)
>
> closestOfTwo :: Point -> Point -> Point -> Point
> closestOfTwo point p1 p2
>   | distance' point p1 < distance' point p2  = p1
>   | otherwise                                = p2

Exercises
---------------------------------------------------------------------------------
1. Define the function "length :: [a] -> Int". It  is  quite  similar to sum  and
   product in the way it traverses its input list. Since length is defined in the
   Prelude, call the new function legnth'.

> length' :: [a] -> Int
> length' []     = 0
> length' (x:xs) = 1 + length' xs

2. What're the values of the following expressions and what's wrong with the ones
   that give errors?

   
   1:[2,3,4]      => [1, 2, 3, 4]
   1:2:3:4:[]     => [1, 2, 3, 4]
   [1,2,3]:[4..7] => The cons operator adds one element, is not the ++ operator.
   [1,2,3] ++ [4..7]   => [1, 2, 3, 4, 5, 6, 7]
   1:['a','b']         => Lists' elements have to be of the same type. 
   "abc"++"cd"         => "abccd"
   "a":"bCc"           => Should be 'a' : "bCc". Trying to ++ two lists.
   "a" ++ "bCc"        => "abCc"
   'a':'b'             => The empty list [] is missing. 
   'a':"b"             => "ab"
   [1,4,7] ++ 4:[5:[]] => Second expression evaluates to a list of different type
   [True,True:[]]      => Idem. List is not the same type as list of lists.
   True:[True,False]   => [True, True, False]
   
3. Write a recursive function fact to  compute th e factorial of a given positive
   number (ignore the case of 0 for this exercise).

   fact n = 1 * 2 * ... * n
   
   Why is the recursive funtion fact a partial function? Add an appropriate error
   case to the function definition.
 
> fact1 :: Integral a => a -> a
> fact1 n = n * fact1 (n - 1)

   fact1 is a partial function because it does not contemplate the fact that:
   0!=1 so the function does not terminate.

> fact2 :: Integral a => a -> a
> fact2 0 = 1
> fact2 n = n * fact2 (n - 1)

4. In the previous chapter, we introduced the ellipsis  list notation in Haskell,
   which allows us to write [m..n] as shorthand for the list [m, m+1, m+2,..., n]
   For numbers m and n, with n greater or equal to m. Write a  recursive  function
   enumFromTo which produces such a list given m and n, such that:

   enumFromTo m n = [m..n]

   As enumFromTo is a Prelude function, you have to call your function enumFromTo'

> enumFromTo' :: Int -> Int -> [Int]
> enumFromTo' m n
>   | m == n    = [n]
>   | n >= m    = [m] ++ enumFromTo' (m + 1) n
>   | otherwise = error "The second element has to be greater than the first"

5. Write a recursive function countOdds which calculate the number of odd elements
   in a list of Int values:

   countOdds [1, 6, 9, 14, 16, 22] => 2

   Hint:  You can  use the Prelude function "odd :: Int -> Bool", which tests whe-
   ther a number is odd.

> countOdds :: [Int] -> Int
> countOdds []  = 0
> countOdds (x:xs)
>    | odd x     = 1 + countOdds xs
>    | otherwise = 0 + countOdds xs
   
6. Write a recursive function removeOdd that, given a list of integers,removes all
   odd numbers from the list, e.g.,

   removeOdd [1, 4, 5, 7, 10] => [4, 10]

> removeOdd :: [Int] -> [Int]
> removeOdd []   = []
> removeOdd (x:xs)
>     | odd x      = removeOdd xs
>     | otherwise  = x : removeOdd xs

7. Challenge: In the definition of the function:
   "closestPoint :: Point -> [Point] -> Point" , the  implementation is less effi-
   cient that one might hope, as it uses the distance' function  twice -instead of
   once- per recursive step.Improve the implementation to avoid that inefficiency.

   type Point = (Float, Float)

   distance' :: Point -> Point -> Float
   distance' (x1, y1) (x2, y2)
     = sqrt (dx * dx + dy * dy)
     where
       dx = x2 - x1
       dy = y2 - y1
       
   closestPoint :: Point -> [Point] -> Point
   closestPoint point [p]    = p
   closestPoint point (p:ps) = closestOfTwo point p (closestPoint point ps)

   closestOfTwo :: Point -> Point -> Point -> Point
   closestOfTwo point p1 p2
     | distance' point p1 < distance' point p2  = p1
     | otherwise                                = p2



> closestPoint' :: Point -> [Point] -> Point
> closestPoint' point [p]   = p
> closestPoint' point (p:ps) = closestOfTwo' point p (closestPoint' point ps)
>     where
>       closestOfTwo' :: Point -> Point -> Point -> Point
>       closestOfTwo' point p1 p2
>         | distance' point p1 < distance' point p2 = p1
>         | otherwise                               = p2



