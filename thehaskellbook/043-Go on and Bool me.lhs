Go on and Bool me
--------------------------------------------------------------------------------
In Haskell the Bool datatype comes standard in the Prelude,and as we saw earlier
Bool is a sum type with two constructors:

data Bool = False | True

This declaration creates a datatype with the type constructor Bool, and we refer
to specific  types by their type constructors.  We use type constructors in type
signatures, not in the  expressions that make up  our term-level code.  The type
constructor Bool takes no arguments (some type constructors do  take arguments).
The definition of Bool above also creates two data constructors, True and False.
Both of  these values are of type Bool. Any function that accepts values of type 
Bool must  allow for the possibility of True or False; you cannot specify in the
type that it should only accept one specific value. An English language formula-
tion of this datatype would be something like: "The datatype `Bool` is represen-
ted by the values True and False".

Boolean Logic
--------------------------------------------------------------------------------
Let's play with infix operators that deal directly with boolean logic. How do we
use Bool and these associated functions?

Prelude> True && True
True

Prelude> False || True
True

The && operator is conjunction, which means "and".The || operator is disjunction
, which means "or".

