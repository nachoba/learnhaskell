The Num Class
--------------------------------------------------------------------------------
2017, Ignacio Matías Sniechowski

Mathematics puts restrictions on the kind of numbers we can add together.  2 + 3
-two  natural  numbers, -7 + 5.12 -a negative integer and a rational number, 1/π 
-a rational an irrational numbers. All of these are valid. In  fact any two real
numbers ℝ can be added together. In order to capture such generality in the sim-
plest way possible  we need a general Number type in Haskell, so that the signa-
ture of the (+) operator would be simply:

(+) :: Number → Number → Number

However, that  design  fits poorly  with the way  computers  perform arithmetic.
While  computers can handle  integers as a sequence of  bits in memory, that ap-
proach does not work for real numbers, thus making it necessary for a less  than
perfect encoding for them: floating point numbers.

  The reason that real numbers are difficult  to represent in computer memory is
  that between any two real numbers there are uncountably many real numbers pos-
  sibly infinite real numbers.

While floating point  provides a reasonable way to deal with real numbers in ge-
neral, it has some inconveniences -most notably, loss of precision- which  makes
using the simpler encoding worthwhile for  integer values. So, we  have at least
two different ways of storing numbers: one for  integers and another for general
real  numbers. Each approach should  correspond to different Haskell types. Fur-
thermore, computers are  only able to perform operations like (+)  on a pair  of
numbers if they are in the same format.

So much for having a universal Number type -it seems that we can't even have (+)
mix integers and floating-point numbers. However,  Haskell can at least  use the
same (+) function with either  integers or  floating point numbers.  We say that
functions can accept arguments of different types if they are made "polymorphic"
,so here's a possible type signature for (+) that  would account  for the  facts
above:

(+) :: a → a → a

With that type signature, (+) would take two arguments of the same type a -which
could be integers or floating-point numbers- and evaluate to a result of type a,
as long as both arguments are the same type. But this signature indicates  "any"
type at all, and we know that we cannot use (+) with two Bool values,or Char va-
lues. What would adding two letters or two truth-values mean? So,the actual type
signature of (+) uses a language feature that allows us to  express the semantic
restriction that a can be any type as long as it is a number type:

(+) :: (Num a) ⇒ a → a → a

Num is a typeclass -a group of types which includes all types which are regarded
as numbers. The "(Num a) ⇒ " part of the  signature restricts a to number  types
-or, in Haskell terminoly, instances of Num.

Numeric types
--------------------------------------------------------------------------------
So, which are the actual number types (that is, the instances of Num that the  a
in the signature may stand for)? The most important numeric types in Haskell are
Int, Integer, Float, and Double.

* Int: Correspond to the plain integer type found in most programming languages.
  It has fixed maximum and minimum values that depend on a computer's processor.
* Integer: Also used for integer numbers, but it supports arbitrarily large  va-
  lues -at the cost of some efficiency.
* Float: Is the single-precision floating point type.
* Double: Is the  double-precision floating point  type, a good  choice for real
  numbers in the vast majority of cases.

Polymorphic guesswork
--------------------------------------------------------------------------------
You know  that we don't need to specify types  because the  Haskell compiler can
infer them.You also know that we cannot mix types when functions require matched
types. Combine this with our new  understanding of  numbers to learn how Haskell
handles basic arithmetic like this:

Prelude> (-7) + 5.12
-1.88

This  may seem to add two numbers of different types -an integer and a non-inte-
ger. Let's see what the types of the numbers we entered actually are:

Prelude> :type (-7)
(-7) :: (Num a) => a

So, (-7) is neither Int nor Integer! Rather,it is a "polymorphic constant",which
can "morph" into any number type. Now, let's look at the other number:

Prelude> :type 5.12
5.12 :: (Fractional a) => a

5.12 is also a polymorphic constant, but one of the Fractional class, which is a
subset of Num (every Fractional is a Num, but not every Num is a Fractional; for
instance, Ints and Integers are not Fractional).When a Haskell program evaluates
(-7) + 5.12, it must settle for an actual matching type for the numbers.The type
inference accounts for the class specifications: (-7) can be Num, but  there are
extra restrictions for 5.12,so that's the limiting factor.With no other restric-
tions, 5.12 will assume the default Fractional type of Double, so (-7) will  be-
come a Double as well. Addition then proceeds normally and returns a Double.  To
check this, try the following in GHCi:

Prelude> x = 2
Prelude> x + 3
5
Prelude> :type (x + 3)
(x + 3) :: Num a => a

Prelude> x + 3.0
5.0
Prelude> :type (x + 3.0)
(x + 3.0) :: Fractional a => a

Monomorphic trouble
--------------------------------------------------------------------------------
The sophistication of the numerical types and classes occasionally leads to some
complications. Consider, for instance, the common division operator (/).  It has
the following type signature:

(/) :: (Fractional a) ⇒ a → a → a

Restricting a to fractional type is a must  because the  division of two integer
numbers will often result in a non-integer.Nevertheless,we can still write some-
thing like:

Prelude> 4 / 3
1.3333333333333333
Prelude> :type (4 / 3)
(4 / 3) :: Fractional a => a

Because the literals 4 and 3 are polymorphic constants and therefore  assume the
type Double at the behest of (/). Suppose, however,we want to divide a number by
the length of a list -which is a reasonable scenario; i.e., computing an average
of the values in a list. The obvious thing to do would be using the length func-
tion:

Prelude> 4 / length [1, 2, 3]

Unfortunately, an error occurs:

<interactive>:32:1: error:
    * No instance for (Fractional Int) arising from a use of `/'
    * In the expression: 4 / length [1, 2, 3]
      In an equation for `it': it = 4 / length [1, 2, 3]

As  usual, the  problem  can be understood  by looking at  the type signature of
length:

length :: [a] → Int

The result of length is an  Int, not a  polymorphic constant. As an Int is not a
Fractional, Haskell won't let us use it with (/). To escape this problem,we have
a special function called fromIntegral, which takes an argument of some Integral
type (like Int or Integer) and makes it a polymorphic constant. 

fromIntegral :: (Integral a, Num b) ⇒ a → b

By combining it with length,we can make the length of the list fit into the sig-
nature of (/):

Prelude> 4 / fromIntegral (length [1, 2, 3])
1.3333333333333333

In some ways,this issue is annoying and tedious,but it is an inevitable side ef-
fect of having a rigorous approach to manipulating numbers. In  Haskell, if  you
define a function with an Int argument, it will never be converted to an Integer
or Double, unless you explicitly use a function like fromIntegral.  As a  direct
consequence of its refined type system, Haskell has  a surprising  diversity  of
classes and functions dealing with numbers.

Classes beyond numbers
--------------------------------------------------------------------------------
Haskell has type classes beyond  arithmetic. For example, the type  signature of 
(==) is:

(==) :: (Eq a) ⇒ a → a → Bool

Like (+) or (/), (==) is a polymorphic function.  It compares  two values of the
same type, which must belong to the class Eq and returns a Bool.Eq is simply the
class for types of values which can be compared for equality,and it includes all
of the  basic non-functional types -comparing two functions for equality is con-
sidered intractable. Typeclasses add a lot to the power of the type system. 

