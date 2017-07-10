Fundamentals
--------------------------------------------------------------------------------
The  functions we defined so far  were restricted to elementary operations, such
as incrementing  a number. In this  chapter, we will  discuss some slightly more
advanced functions and survey elementary operations on lists.

Programs are Composed from Modules
Usually, a program  consists of a large number of function and type definitions.
Obviously, putting  them all into one single file is a bad idea. Modern program-
ming languages  therefore provide some  means to structure  the program by allo-
wing to group related definitions into logical units which are stored in separa-
te files. In Haskell, these units are called "modules".
Let us have a look at the definition of a Haskell module, called Simple:

> -- This is a simple example of a module definition
> module Simple where
>
> -- calculates the areithmetic mean of two numbers
> arithmeticMean :: Fractional a => a -> a -> a
> arithmeticMean x y = (x + y) / 2
>
> -- calculates the harmonic mean of two numbers
> harmonicMean :: Fractional a => a -> a -> a
> harmonicMean x y = 2 * x * y / (x + y)


The module begins  with a header: a comment containing a one line description of
the module, the author and date of  creating, and briefly  describes the purpose
of the module. The first line of code starts with the keyword "module", followed
by the name of the module, the keyword "where", and the definitions  that belong
to the module.Note that a module name, in contrast to function or variable names
"must" start with an uppercase letter.
In Haskell, there is a special module called Prelude whose contents is always a-
vailable.  The module Prelude contains all the functions that are pre-defined in
Haskell, such as +, length, and so on. For now,  as we are starting with simple,
short  programs, we will  add all function definitions  of a program to a single
module.  Later we will learn  how we can structure  more complex  programs using
multiple modules.  In fact, this is a central topic in software development, and
modules play an essential role in structuring large software systems.

Branches in the Control Flow
--------------------------------------------------------------------------------
So far, each  of our functions has unconditionally performed one specific compu-
tation. In general, this is too limiting; so, next, we will  look at the role of
conditionals in Haskell.

* Choice by way of conditionals: How can we implement a function "max" which re-
  turns the greater of its two arguments? That is, the expressions "max 5 2" and
  "max 2 5" both evaluate to 5,  "max 1 7" to 7,  and so on.   For two arbitrary
  numbers  x  and  y, we want  "max x y" to return  "x if x >= y"; otherwise, it
  should return y.

We can generally compare values  with types that  belong to the type  class Ord,
which we discussed earlier; hence, the type of max is:
                                                     max :: Ord a => a -> a -> a

This can be expressed in Haskell by a so-called  "conditional" or  "if-then-else
expression" of the form:
                       if (condition) then <value if true> else <value if false>

Now, we can implement max:

> max' :: Ord a => a -> a -> a
> max' x y = if x >= y then x else y

Let's look at the evaluation of "max 5 2":

max 5 2   =>  if 5 >= 2 then 5 else 2  => if True then 5 else 2  => 5

Conditionals are an essential component of programming, because they allow us to
dynamically choose between different computations depending on the values of the
inputs. Here is another example:

> signum' :: (Ord a, Num a) => a -> Int
> signum' x = if x < 0 then (-1) else
>               if x == 0 then 0 else 1

A note about the type of signum': the type of  the argument x has to  be both in 
Ord, as  we use the functions < and ==, but  also in Num, otherwise we could not
be able to  compare it to 0, which is  in the type class Num. We will cover type
classes in more depth in a later chapter.



* Guards: Cascading  conditional expressions -as in  the previous  definition of 
  signum'- are difficult  to read; therefore, some programming languages, inclu-
  ding Haskell, provide an alternative syntax:

> signum'' :: (Ord a, Num a) => a -> Int
> signum'' x  | x <  0  = -1
>             | x == 0  =  0
>             | x >  0  =  1

The guards  are checked in the order  they are listed.  For example, if we apply 
signum'' to the number 7, then the system first  checks if the  argument is less
than zero.As this is not the case, it checks whether it is equal to zero, before
finally the last guard succeeds:

signum'' 7   =>  7 <  0  => False
             =>  7 == 0  => False
             =>  7 >  0  => True
             =>  1


Usually, the  last guard should catch all the cases not covered before. For this
purpose,  we can  use  the special  guard "otherwise", which always evaluates to
True.

> signumO :: (Ord a, Num a) => a -> Int
> signumO x  | x <  0     = -1
>            | x == 0     =  0
>            | otherwise  =  1

Finally, we can rephrase the definition of max using guards:

> max'' :: (Ord a) => a -> a -> a
> max'' x y | x >= y     = x
>           | otherwise  = y

Which to  choose is often a matter of personal style; however, idiomatic Haskell
tends to favour code using guards.

Binders - Associating Names with Values or Functions
--------------------------------------------------------------------------------
A "binder" binds a value to a name. The value can subsequently be referred to by
that name. For example, the Prelude definition:

 pi :: Floating a => a
 pi = 3.141592653589793

Allows us to just write "pi" instead  of  spelling  out "3.141592653589793". The
type class  Floating contains the types Float and Double (i.e. single and double
precision floating point numbers). We may use a previously introduced name in a-
nother function definition:

> circleArea :: Floating a => a -> a
> circleArea radius = pi * radius ^ 2

Sometimes, we  need to introduce a  new name, which will  only be used withing a
function. In that case, we should use a "local binding". For example,

> circleArea' :: Floating a => a -> a
> circleArea' diameter = pi * radius ^ 2
>    where
>    radius = diameter / 2.0    -- local binding

The evaluation of this function proceeds as follows:

circleArea' 6.0  => pi * radius ^ 2 where radius = 6 / 2.0
                 => pi * radius ^ 2 where radius = 3.0
                 => pi * 3.0 ^ 2
                 => pi * 9.0
                 => 3.141592653589793 * 9.0
                 => 28.274333882308138


Tuples: Combining Different Data Items
--------------------------------------------------------------------------------
So  far, we have seen  how to pass multiple  values to a function, but not how a
function can return more than one result value.We can achieve this by using "tu-
ples":

> addMul :: Num a => a -> a -> (a, a)
> addMul x y = (x + y, x * y)

A tuple combines  multiple components (two integer values, in the above example)
into one "compound" value. The compound value can be manipulated as a single en-
tity and, in particular, be  returned as a value form a  function.  However, the 
"construction" of a compound value is only half of the story. We also need a me-
thod for "decomposing" such values. We achieve this by using a  notation dual to
that of tuple construction.

fst (x, y) = x
snd (x, y) = y

In the argument of fst, we do  not use a variable to refer to the compound argu-
ment as a whole. Instead, we decompose the pair of components x and y this is so
called "decomposition by pattern matching". The combined use of "addMul" and fst
behaves as follows:

 fst (addMul 5 6)   => fst (5 + 6, 5 * 6)
                    => fst (11, 30)
                    => 11

It is  interesting to inspect the  types of fst and snd. So far, we  omitted the
type annotations from the definitions of fst and snd.Generally, the Haskell sys-
tem will automatically infer the types of definitions that lack a signature how-
ver, it is  good style to explicitly provide signatures. In all our previous de-
finitions,the type was determined by the operations we applied to the arguments.
For example  max was  restricted to work on types which  are members of the type
class Ord, since we needed the operator >=, which can only compare certain types
of values.  The functions fst and snd are  different, though.  We don't actually
apply any function or operation to x or y, so they  could be values of any type.
In fact, x and  y don't even  have to have the same type. Hence, we can just use
type  variables to represent the types of x and y without having to add any type
class restrictions:

fst :: (a, b) -> a
fst (x, y) = x

snd :: (a, b) -> b
snd (x, y) = y

Functions like  fst and snd, which can handle  arguments of any  type are called
(parametric) polymorphic functions.

* Example: Tuples are not just useful  to return  multiple results, but also for
  POINTS   the representation of data items that cannot be modeled by one primi-
           tive  value alone. A useful example is given by the points in a 2-di-
           mensional Cartesian coordinate system:  they can be represented  by a
           pair of integer values. To avoid having to write the less informative
           (Int, Int) whenever we denote the type of a point, we can introduce a
           new  type name  similar to the  introduction of names for  repeatedly 
           used values, which we discussed earlier:


> type Point = (Int, Int)

With this definition, we define some simple operations on points:

> -- origin of the coordinate system
> origin :: Point
> origin = (0, 0)
>
> -- move a given point to the right
> moveRight :: Point -> Int -> Point
> moveRight (x, y) distance = (x + distance, y)
>
> -- move a given point to the left
> moveLeft :: Point -> Int -> Point
> moveLeft (x, y) distance  = (x, y + distance)


* Example: When we extend points to include a colour, another important property
  COLOUR   of tuples becomes obvious:tuple components may be of different types.
  POINTS   Hence, if we denote colour values with a textual (string) representa-
           tion, we have:
  
> -- we represent colours by strings
> type Colour = String
>
> -- new name for the type of colour points
> type ColourPoint = (Int, Int, Colour)

Which enables the following operations on colour points:

> -- origin of the coordinate system in a given colour
> originColour :: Colour -> ColourPoint
> originColour colour = (0, 0, colour)
>
> -- move a colour point vertically and horizontally
> moveColour :: ColourPoint -> Int -> Int -> ColourPoint
> moveColour (x, y, colour) xDistance yDistance =
>     (x + xDistance, y + yDistance, colour)
>    
> -- compute the distance between two colour points
> distance :: ColourPoint -> ColourPoint -> Float
> distance (x1, y1, colour1) (x2, y2, colour2) =
>     sqrt (fromIntegral (dx * dx + dy * dy))
>        where
>        dx = x2 - x1
>        dy = y2 - y1

Note how we use a "where" clause in  the last definition to  avoid repeating the
expressions "x2 - x1" and "y2 - y1". The standard function fromIntegral converts
any integral type to any other numeric type. Its signature is:

fromIntegral :: (Integral a, Num b) => a -> b

* Important symmetries in Haskell: If  we compare the syntax of values and types
  of tuples, we see that they correspond. For example, consider

  (10, 15, "green") :: (Int, Int, String)

  If we replace  the values 10, 15, and "green" with their respective types Int,
  Int, String, we obtain  the type of the tuple.  Moreover, we have a correspon-
  dence between "term construction" and "term decomposition"  (also called "pat-
  tern matching"). Consider,
  
  startPoint = (0, 0, "black")
  colourOfPoint (x, y, colour) = colour

If we replace the components in the tuple construction (0, 0, "black") by varia-
ble names (in this case x, y, colour), we arrive at the pattern that can be used
to decompose the corresponding tuple.

* Special  names for some tuples: The  following table  lists a  number of tuple
  types and their names:

   | No. of Items | Expression             | Name      |
   | ------------ | ---------------------- | --------- |
   |   0          |  ()                    | Unit      |
   |   1          |  n/a                   | n/a       |
   |   2          |  (x1, x2)              | Pair      |
   |   3          |  (x1, x2, x3)          | Triple    |
   |   4          |  (x1, x2, x3, x4)      | Quadruple |
   |   5          |  (x1, x2, x3, x4, x5)  | Quintuple |
   |  ...         |  ...                   | ...       |
   |   n          |  (x1, x2, x3, … , xn)  | n-Tuple   |


Lists: Many Values of a Single Type
--------------------------------------------------------------------------------
Tuples provide the ability to bring together a fixed number of values of varying
type. However, many applications, in addition, require the ability to manipulate
compound  types that may contain a varying  number of elements of a single type.
This is what lists are for. We can create lists in a manner similar to pairs, by
listing all components, separated by a comma. The only difference is that we en-
close these values in square brackets:

> firstTenPrimes :: [Int]
> firstTenPrimes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 27]

Lists are a central  data structure in Haskell, and the language offers a lot of
convenient short cuts. We don't need to explicitly write every single element if
our list elements  are just a  sequence of consecutive  numbers -or in fact, any
type whose values can be enumerated:

> oneToTwenty :: [Int]
> oneToTwenty = [1..20]

If the elements follow a simple pattern of the form:
                                             [n, n + k, n + 2*k, n + 3*k, …., m]
We simply write  the first two numbers  and the last one, and  the compiler will 
figure out what the elements are:

> -- return all positive odd numbers up to maxNumber
> oddNumbers :: Int -> [Int]
> oddNumbers maxNumber = [1, 3 .. maxNumber]

The length of the list returned by this function depends on the argument, unlike
with the earlier addMul, where only the value, but not the number of values, de-
pended on the input. For example, we have:
oddNumbers 10  => [1, 3, 5, 7, 9]
oddNumbers 15  => [1, 3, 5, 7, 9, 11, 13, 15]

The difference between tuples and lists can be seen by comparing their types, as
in:
(1, 2, "green") :: (Int, Int, String)
[1, 2, 3, 4, 5] :: [Int]

The number of components is explicit in the type of a tuple, but not in the type
of a list.As a consequence, the elements of tuples may be heterogeneous, whereas
those of lists must be homogeneous.

Useful Functions on Lists
--------------------------------------------------------------------------------

* Adding elements to the front of the list: The  right-associative  operator (:)
  adds another element to the fron of an existing list:
  (:) :: a -> [a] -> [a]

  For example, we have:
  "blue" : []                     => ["blue"]
  "yellow" : ["blue"]             => ["yellow", "blue"]
  "red" : ["yellow", "blue"]      => ["red", "yellow", "blue"]
  "red" : "yellow" : "blue" : []  => ["red", "yellow", "blue"]

  The operator (pronounced cons) can only add elements at the front. So,
  ["red", "yellow"] : "blue"      => Error!

  The cons operator is another example of a polymorphic function, as it works on
  lists of any type.  The only restriction is that the  element we are adding is
  of the same type as the elements of the list we pass as the second argument.

* Joining two lists: We can join two lists together using the (++) operator:
  (++) :: [a] -> [a] -> [a]

  Just as the cons operator, it works  on lists of any type, as long  as the two
  lists have the same type of elements:
  [4, 2, 3] ++ [3, 1, 2, 7]       => [4, 2, 3, 3, 1, 2, 7]
  [True] ++ [False, True]         => [True, False, True]

  You may remember this operator from the  previous chapter, where we used it to
  join two strings to define:

> exclaim :: String -> String
> exclaim sentence = sentence ++ "!"

  This works, because strings are simply lists of character in Haskell.String is
  a type alias of [Char].

* Extract the element at a specific  index position out of a list: We can  index
  into a list and extract an element using the !! operator:

  (!!) :: [a] -> Int -> a

  On lists of integers:
  [0, 1, 2, 3] !! 2      => 2

  or on strings (lists of characters):
  "Hello" !! 4           => 'o'

  As you can see, the index count starts at 0. If we apply the index operator to
  an index which is out of range for the given list,evaluation will raise an ex-
  ception and we get an error message.

* Split a list into its first  element and the rest: A list  consists of a  head
  and a tail. The  head is the first  element of a list, whereas the tail is the
  rest of the list.

  head :: [a] -> a
  tail :: [a] -> [a]


  Both functions require the input list to be non-empty, and if they are applied
  to an empty list, we get a runtime error.

  head [0, 1, 2, 3]   => 0
  tail [0, 1, 2, 3]   => [1, 2, 3]
  head "mouse"        => 'm'
  tail "mouse"        => "ouse"


* Length of a list: The get the length of a list we use the length function:

  length :: [a] -> Int

  For example:

  length [0, 1, 2, 3]   => 4
  length "hello"        => 5


* Check if an item is contained in a list: We can check if an item is element of
  a given list if its type is in the class Eq. This restriction is necessary be-
  cause the implementation of this function needs to compare the items for equa-
  lity.

  elem :: Eq a => a -> [a] -> Bool

  For example, we have:

  elem 2 [0, 1, 2, 3]   => True
  elem 5 [0, 1, 2, 3]   => False
  elem 'o' "Hello"      => True

This function can also be used in *infix* notation:

2 `elem` [0, 1, 2, 3] => True
'o' `elem` "Hello"    => True

* Add up or multiply the elements of a list: We can calculate the maximum, mini-
  mum, sum, or the product of all the elements of alist with these functions:

  maximum :: Ord a => [a] -> a
  minimum :: Ord a => [a] -> a
  sum     :: Num a -> [a] -> a
  product :: Num a => [a] -> a

For example, we get:

  sum [0, 1, 2, 3]      => 0 + 1 + 2 + 3  => 6
  product [1, 2, 3, 4]  => 1 * 2 * 3 * 4  => 24

Given  these  functions, how can  we add "yellow" at the end of ["red", "green", 
"blue"]? We cannot use the (:) cons operator, since it can only add  elements at
the start of a list. We need to use ++ as follows:

["red", "green", "blue"] ++ ["yellow"]

Note the list constructing  brackets [] around "yellow". In effect, we wrap "ye-
llow"  into a singleton list, which we then  append to the list ["red", "green",
"blue"].

Lists versus Tuples
--------------------------------------------------------------------------------
As lists and tuples are often confused, let us summarise the differences between
them. 

* Tuples have the following properties:

* Fixed size, i.e., fixed number of components: the  pair (1,2) :: (Int,Int) and
  the triple (1,2,3) :: (Int,Int,Int) have different types.
* Components may be of different types: (5, "Hello") :: (Int, String)

* Lists have the following properties:

* Variable  size,  i.e., number  of  components  may  vary:  [1, 2] :: [Int] and 
  [1, 2, 3] :: [Int] have the same types.
* Components must be of the same type [5, "Hello"] is incorrect.

Strings as Lists
--------------------------------------------------------------------------------
Strings are in fact a particular form of lists in  Haskell, which are defined as
follows in the Prelude:

type String = [Char]

Hence, list operations work on strings. "Hello" !! 1 gives 'e'. In fact, in Has-
kell "Hello" is exactly the same as ['H', 'e', 'l', 'l', 'o']. This is very con-
venient, as we will see that there are many powerful  list processing operations
and these are directly  available for string  manipulation. Hence, if we ask the
Haskell system for the type of an expression that produces a list, it may report
the type as either String or as [Char].As these two types have been deemed equal
by the above type definition, they may be used interchangeably.

Partial Functions
--------------------------------------------------------------------------------
Some list  functions, such as length, may be applied to any list and always pro-
duce a meaninful result.In contrast,some other functions, such as head and tail,
fail for some lists.  For example, if  we apply head to an  empty list, we get a 
runtime error or runtime exception.

head []    =>  ***Exception: Prelude.head: empty list

The function head is defined by pattern matching using the same symmetry between
list  construction and  list pattern matching as we discussed previously for tu-
ples -i.e., it matches on the cons operator (:):

> head' :: [a] -> a
> head' (x:xs) = x

When head gets applied to the empty list [],there is simply no function equation
that  matches this case. We call such functions partial functions. Their defini-
tion ignores -or is  invalid for- some of the input values admitted by the func-
tion's type signature. We discussed that types represent sets of related values;
hence, partial functions are functions that are only defined for a subset of the
values included in their argument types. The opposite are total functions. Total
functions, such as length,are defined for all values included in the sets repre-
sented by their argument types.

Partial functions are a common source of programming mistakes. Hence, good style
dictates that the documentation of a partial function (e.g., as a comment prece-
ding the  function definition) includes  a statement specifying the range of ad-
missible input values (and specifies what happens if the function is incorrectly
called with an argument that is outside of this range).

The Haskell Prelude includes the function:

error : String -> a

That always results in a runtime error using its argument as the error message:

error "Fatal Error!"   => *** Exception: Fatal Error!

We can use error to customise the error message of a partial function. For exam-
ple, the complete definition of head in the Haskell Prelude reads as follows:

> headPrelude :: [a] -> a
> headPrelude (x : _) = x
> headPrelude []      = error "Prelude.head: empty list"

This is better than the default error message generated by the Haskell compiler,
which includes the location of the failing function definition, but doesn't spe-
cify the argument value that led to the failure.

Note the  use of the underscore '_' as the second  argument of the cons-operator
in the first equation. It represents a patter matching position  whose  value is
not used in the body of the function definition. It is better style than using a
regular variable name, such as xs, as it immediately signals that the correspon-
ding value goes unused. Instead of a plain undersocre, we can also use  variable
names starting with  an underscore character (here, for example _xs) to indicate
that the value is not used.

Layout
--------------------------------------------------------------------------------
Unlike in many other programming languages, formatting matters in Haskell. In o-
ther  words, the correct use of indentation and newlines is crucial to avoid er-
rors.  This allows the language to do away with some of the noise that is intro-
duced by some  other languages to disambiguate the input -in particular, Haskell
programs don't need curly braces or semicolons to delimit blocks and statements.

Compare

foo x
  = a + b
  where
    a = 1 + x
    b = 2


to


foo x
  = a + b
  where
    a = 1 + x
 b = 2

Both are legal programs. However, in the first one, the  definition of b is part
of the where binding and therefore local to foo  whereas in the second  program,
the use of b is  not restricted to foo. An example of proper layout is the func-
tion distance that we discussed earlier:

> distance' :: ColourPoint -> ColourPoint -> Float
> distance' (x1, y1, colour1) (x2, y2, colour2) 
>   = sqrt (fromIntegral (dx * dx + dy * dy))
>   where
>     dx = x2 - x1
>     dy = y2 - y1

There are three layout rules that we have to follow to get syntactically correct
programs:

1. All  program code that  belongs to a function definition has to be further to
   the right than the first character of that definition (i.e.,the first charac-
   ter of the function name). In  the case of distance', all code has to be fur-
   ther to the right than the column in which the character 'd' of  the function
   name distance' is located.
2. Similarly, all  code of a local definition  in a where clause must be further
   to the  right than the first character of the name of the variable defined by
   the binding.
3. All definitions within  a where clause must be aligned -e.g., above the defi-
   nitiions of dx and dy start in the same column.

Alternatively,we can explicitly group the bindings of a where clause using curly
braces, and separate the individual bindings by semicolons

distance (x1, y1, colour1) (x2, y2, colour2)
  = sqrt (fromIntegral (dx * dx + dy * dy))
  where {dx = x2 - x1 ; dy = y2 - y1}

Inside curly braces, we can format the code anyway we like, because the compiler
can still  figure out what belongs where. Nevertheless, while the following pro-
gram is legal

distance (x1, y1, colour1) (x2, y2, colour2)
  = sqrt (fromIntegral (dx * dx + dy * dy)) where { dx 
= x2 
- x1 ; dy = y2 - y1}

It  is hard to read and  clearly pretty  bad style. Seasoned Haskell programmers
who prefer explicit bracketing align the braces and the semicolons at the begin-
ning of each  line (which looks  rather strange  to, say, a C programmer, who is
used to having semicolons at the end of the line):

distance (x1, y1, colour1) (x2, y2, colour2)
  = sqrt (fromIntegral (dx * dx + dy * dy))
  where
    { dx = x2 - x1
    ; dy = y2 - y1
    }

Exercises
--------------------------------------------------------------------------------
1. Write a function "sort2 :: Ord a => a -> a -> (a, a)"  which accepts  two Int
values as arguments  and returns  them as a  sorted pair, so that "sort2 5 3" is
equal to (3, 5). How can you define the function using a conditional,how can you
do it using guards?

> -- using a conditional
> sort2 :: Ord a => a -> a -> (a, a)
> sort2 x y = if x < y then (x, y) else (y, x)
>
> -- using guards
> sort2' :: Ord a => a -> a -> (a, a)
> sort2' x y
>      | x < y     = (x, y)
>      | otherwise = (y, x)

2. Consider a function: "almostEqual :: Eq a => (a, a) -> (a, a) -> Bool"  which
compares the values of two pairs. It returns True if both pairs contain the same
values, regardless of the order. For example, "almostEqual (3,4) (4,3)" is True,
but "almostEqual (3,4) (3,5)" is False.Which of the following definitions return
the correct value? Which of the definitions would you consider good style? Why?

> -- first version: works fine, reads relatively good
> almostEqual :: Eq a => (a, a) -> (a, a) -> Bool
> almostEqual (x1, y1) (x2, y2)
>       | (x1 == x2) && (y1 == y2)  = True
>       | (x1 == y2) && (y1 == x2)  = True
>       | otherwise                 = False
>
> -- second version: works fine, reads better that the previous
> almostEqual' :: Eq a => (a, a) -> (a, a) -> Bool
> almostEqual' (x1, y1) (x2, y2)
>       | (x1 == x2) = (y1 == y2)
>       | (x1 == y2) = (y1 == x2)
>       | otherwise  = False
>
> -- third version: works fine, more difficult to read
> almostEqual'' :: Eq a => (a, a) -> (a, a) -> Bool
> almostEqual'' pair1 pair2
>     = (pair1 == pair2) || (swap pair1 == pair2)
>       where
>           swap (x, y) = (y, x)
>
> -- fourth version: does not work.
> almostEqual''' :: Eq a => (a, a) -> (a, a) -> Bool
> almostEqual''' pair1 pair2
>       = (pair1 == pair2) || (swap pair1 == swap pair2)
>       where
>           swap (x, y) = (y, x)
>
> -- fifth version: works fine, horrible reading
> almostEqual4 :: Eq a => (a, a) -> (a, a) -> Bool
> almostEqual4 (x1, y1) (x2, y2)
>     = if (x1 == x2) 
>         then
>           if (y1 == y2) 
>             then True
>             else False
>         else 
>           if (x1 == y2) 
>             then 
>               if (x2 == y1)
>                 then True
>                 else False
>             else False

3. Define a function  "isLower :: Char -> Bool"  which returns  True if  a given
character is a lower case letter. You can use the fact that characters are orde-
red, and for all lower case letters "ch" we have 'a' <= ch and ch <= 'z'. Alter-
natively, you  can use the fact  that ['a'..'z'] evaluates  to a list containing
all lower case letters.

> isLower :: Char -> Bool
> isLower char = char `elem` ['a'..'z']

4. Write a  function  "mangle :: String -> String"which removes the first letter
of a word  and attaches it  at the end.  If the string  is empty, mangle  should
simply return an empty string:

   mangle "Hello"    => "elloH"
   mangle "I"        => "I"
   mangle ""         => ""


> mangle :: String -> String
> mangle word
>       | null word        = ""
>       | length word == 1 = word
>       | otherwise        = tail word ++ [head word]

5. Implement division on Int, "divide :: Int -> Int -> Int" using the list func-
tions described  in this section. Hint: first, write a function that returns all
the multiples of a given number up to a specific limit.

   divide 5 10     => 2
   divide 5 8      => 1
   divide 3 10     => 3

> mult :: Int -> Int -> [Int]
> mult number limit = [1 * number, 2 * number .. limit * number]
>
> divide :: Int -> Int -> Int
> divide x y = length [ a | a <- (mult x y), a <= y]

