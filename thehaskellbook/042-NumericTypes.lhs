Numeric Types
--------------------------------------------------------------------------------
It is important to undestand that Haskell does not just use one  type of number.
For most purposes, the types of numbers we need to be concerned with are:

Integral numbers: These are whole numbers, positive and negative.

  1. Int     : This type is a fixed-precision integer. By "fixed  precision", we
              mean it has a range, with a maximum and a minimum.
  2. Integer : This type  is also for integer, but  this  one supports arbitrary
               large (or small) numbers.

Fractional numbers: These are not integers.Fractional values include the  follo-
wing four types:

  1. Float      This is the type  used for single-precision floating point  num-
                bers.  Fixed-point number representations  have immutable number 
                of digits assigned for the parts  of the number before and after
                the decimal point.  This  flexibility  mean that  floating point
                arithmetic violates some  common  assumptions  and  should  only
                be used with great care.
  2. Double     This is  a double-precision  floating point  number type. It has 
                twice  as many  bits  with  which  to describe  numbers  as  the 
                Float type.
  3. Rational   This is a fractional number that represents a ratio of two inte-
                gers. The value   1 / 2 :: Rational   will  be a value  carrying
                two Integer values, the numerator 1 and  the  denominator 2. Ra-
                tional is arbitrarily precise but not efficient as Scientific.
  4. Scientific This is a space  efficient and almost-arbitrary precision scien-
                tific  number  type. Scientific  numbers are  represented  using 
                scientific notation. It stores the coefficient as an Integer and
                the exponent as an Int. Since Int is not arbitrarily-large there
                is technically a  limit to  the size of number  you can  express 
                with  Scientific.  This  numeric type is available  in a library
                with the same name.

All these numeric datatypes  all have instances of a type class called Num. Type
classes  are a way of adding functionality to types that is reusable  across all
the types that have instances of that type class. Num is a type class for  which 
most  numeric types will have an instance because there are  standard  functions 
that are  convenient to have available  for all types  of numbers. The  Num type 
class is what  provides (+), (-), and (*) operators. An instance defines how the
functions work for that specific type.

Integral Numbers
--------------------------------------------------------------------------------
There are  two types of integral numbers: Int, and Integer. Integral numbers are 
whole numbers with no fractional component. The following are integral numbers: 
                   1       2     199    32435584848483929234 
While the following are not integral numbers: 
                   1.314   1/2   -2.1   3.14159267

You can  find out the  minimum and  maximum  bounds of numeric  types  using the
 maxBound and minBound from the Bounded type class:

Prelude> import GHC.Int
Prelude> :type minBound
minBound :: Bounded a ⇒ a

Prelude> :type maxBound
maxBound :: Bounded a ⇒ a

Prelude> minBound :: Int8
-128

Prelude> minBound :: Int16
-32768

Prelude> minBound :: Int32
-2147483648

Prelude> minBound :: Int64
-9223372036854775808

You can find  out if a type has instance of Bounded, or any other type class, by
asking GHCi for the ":info" for that type. Doing this will also give you the da-
tatype representation for the type you queried:

Prelude> :info Int
data Int = GHC.Types.I#  GHC.Prim.Int#
instance Bounded Int -- Defined in 'GHC.Enum'

Fractional Numbers
--------------------------------------------------------------------------------
Of the four common  Fractional types used in  Haskell, Rational, and  Scientific
are arbitrary precision. Arbitrary precision means  that these can be used to do 
calculations requiring a high degree of precision rather than being limited to a
specific degree of precision like Float, and Double are.
Some computations involving numbers will be fractional rather than integral:

Prelude> :type (/)
(/) :: Fractional a ⇒ a → a → a

The notation "Fractional a ⇒ " denotes a typeclass  constraint. You  can read it
as "the type variable "a" must implement the "Fractional" type class".Fractional
is a typeclass that  requires types to  already have an instance of the Num type 
class. We describe this relationship between typeclasses by saying that Num is a
superclass of Fractional.

Comparing Values
--------------------------------------------------------------------------------
In Haskell we have the usual operators for comparing values: 

* ==  is equal to
* /=  not equal to
* >   greater than
* <   less than
* <=  less or equal than
* >=  greater or equal than

GHCi returns a result of either True or False, depending on whether the  expres-
sion is true or false. True and False are the data constructors for the Bool da-
tatype.  If we look at the type information for any of these infix operators, we
will find the result type listed as Bool:

Prelude> :type (==)
(==) :: Eq a ⇒ a → a → Bool

Prelude> :type (<)
(<) :: Ord a ⇒ a → a → Bool

Notice that we get some type class constraints again. Eq is a typeclass that in-
cludes everything that can be compared  and determined to be equal in value; Ord
is a type class that includes all  things that can be ordered. Note that neither 
of these is  limited to numbers. Numbers can be compared and ordered, of course,
but so can letters, so this type class constraint allows for flexibility.
A datatype that has no instance of Ord will not work with these functions:

> data Mood = Blah | Woot
>             deriving Show

No try these in the REPL:

Prelude> [Blah, Woot]
[Blah, Woot]
Prelude> [Blah, Woot] > [Woot, Blah]
<interactive>:3:1: error:
    * No instance for (Ord Mood) arising from a use of `>'
    * In the expression: [Blah, Woot] > [Woot, Blah]
      In an equation for `it': it = [Blah, Woot] > [Woot, Blah]

"No instance for (Ord Mood)" means it does not have an Ord instance to  know how
to order these values.
