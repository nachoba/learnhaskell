Types of concatenation functions
--------------------------------------------------------------------------------
Let us have a look at the types of (++) and  concat. The ++ function is an infix
operator.  When we need to refer to  an infix operator in a position that is not
infix (such as when we are using it in a prefix position), we must put parenthe-
ses around it.  On the other hand, concat is a normal  (not infix)  function, so
parentheses aren't necessary.

Prelude> :type (++)
(++) :: [a] → [a] → [a]

Prelude> :type concat
concat :: Foldable t ⇒ t [a] → [a]

> numericList :: [[Integer]]
> numericList = [ [1, 2]
>               , [3, 4, 5]
>               , [6, 7]
>               ]

Prelude> concat numericList
[1,2,3,4,5,6,7]

> stringList :: [String]
> stringList = [ "Buenos Aires "
>              , "Entre Rios "
>              , "Santa Fe "
>              ]

Prelude> concat stringList
"Buenos Aires Entre Rios Santa Fe"

So let's examine the type signature of (++):

Prelude> :type (++)
(++) :: [a] → [a] → [a]

    	(1)   (2)   (3)

Everything after the  ::  is about our types, not our values. The 'a' inside the
list type constructor [] is a type variable.

(1) Take an argument of type [a]: This  type is a list of  elements of some type
    'a'. This function does not know what type 'a' is. It does not need to know.
     In the context of the  program, the type of 'a' will be known and made con-
     crete at some point.

(2) Take another argument of type [a], a  list of elements whose type we  do not
    know.Because the variables are the same, they must be the same type through-
    out (a == a).

(3) Return a result of type [a].

Because String is a type of list, the operators we  use with strings can also be
used on lists of other types, such as lists of numbers.  The type [a] means that
we have a list  with elements of a type 'a' we do not yet know. If we use the o-
perators to concatenate lists of numbers, then the 'a' in [a] will be  some type
of number (for example, integers).  If we are concatenating lists of characters,
then 'a' represents a  Char because String  is [Char].  The type variable 'a' in
[a] is "polymorphic". Polymorphism is an important feature of Haskell.
For  concatenation, every list must be the same type of list; we cannot concate-
nate a list of numbers with a list of characters.

Prelude> "hello" ++ " Nacho"
"hello Nacho"

Prelude> "hello" ++ [1, 2, 3]
error:

     No instance for (Num Char) arising from the literal ‘1’
     In the expression: 1
     In the second argument of (++), namely ‘[1, 2, 3]’`
     In the expression: "hello" ++ [1, 2, 3]`

