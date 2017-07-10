Tuples
--------------------------------------------------------------------------------
The tuple in Haskell is a type that allows you to store and pass around multiple
values within a single value. Tuples have a distinctive,  buit-in syntax that is
used  at both type and term levels, and each tuple has a fixed number of consti-
tuents. 
We refer to tuples by how many constituents are in each tuple: 

* the two-tuple or pair
* the three-tuple or triple
* etc.

This number is also  know as the tuple's arity. The values within a tuple do not
have to be of the same type.
We will start by looking at the two-tuple, a tuple with two constituents.The two
tuple is expressed  at both the type level  and term level with  the constructor 
( , ). The datatype declaration looks like:

Prelude> :info (,)
data (,) a b = (,) a b  -- Defined in `GHC.Tuple'

This  is different from the Bool type we looked at earlier in a couple of impor-
tant ways, even apart from the special type constructor syntax. 

1. The first is that it has two parameters,represented by the type variables 'a'
   and 'b'. Those have to be applied to concrete types, much as variables at the
   term level have to be applied to values to evaluate a function.
2. The second major difference  is that this is a "product type", and not a "sum
   type".A "product type" represents a logical conjunction: you must supply both
   arguments to  produce a value. Notice  that the two type variables are diffe-
   rent, so that allows  for tuples that contain  values of two different types. 
   The types, however, are not *required* to be different.

Prelude> (,) 8 10
(8,10)
Prelude> (,) 8 "Nacho"
(8,"Nacho")
Prelude> (,) True 'c'
(True,'c')

But if we try to apply it to only one argument:

Prelude> (,) 9
<interactive>:21:1: error:
    * No instance for (Show (b0 -> (a0, b0)))
        arising from a use of `print'
        (maybe you haven't applied a function to enough arguments?)
    * In a stmt of an interactive GHCi command: print i

