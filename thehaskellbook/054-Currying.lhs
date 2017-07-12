Currying
--------------------------------------------------------------------------------
As in the lambda  calculus,  arguments (plural) is a  shorthand for the truth in
Haskell: all functions in Haskell take one argument and return one result. Other
programming languages, if you have any experience with them, typically allow you
to define functions that can take multiple arguments.   There is no support  for
this built into Haskell. Instead there are syntactic conveniences that construct
curried functions by default.   Currying refers to the nesting of multiple func-
tions, each accepting one argument and returning one result,  to allow the illu-
sion of multiple-parameter functions. The arrows we have seen in type signatures
denote the function type.  We looked at the datatype definition earlier, but let
us review:

                 data (→) a b

In order to have a function, you must have one input, the a, to apply  the func-
tion to, and you'll get one result, the b, back.  Each arrow in a type signature
represents one argument and one result,  with the final type being the final re-
sult. If you are constructing a function that requires multiple parameters, then
the b can be another function  (the a can be another function as well,  which we
will look at more later).   In that case,  just like in lambda abstractions that
have multiple heads, they are nested.  Let us break this down by looking  at the 
type signature for addition, a function that needs multiple inputs:

                 (+) :: Num a ⇒ a → a → a
                        -[1]-   -[2]-  [3]

[1]. Here's the typeclass constraint saying that a must have an instance of Num.
     Addition is defined in the Num typeclass.
[2]. The boundaries  demarcate what  you might  call the two  parameters to  the
     function (+), but all functions in Haskell take one argument and return one
     result.  This is  because functions  in Haskell are nested  like Matryoshka
     dolls in  order to accept "multiple" arguments.  The way the (→) type cons-
     tructor for functions works means "a → a → a" represents successive  funct-
     ion application, each taking one argument and returning one result.The dif-
     ference is that the  function at the  outermost layer is actually returning
     another function that accepts the next argument. This is called currying.
[3]. This is the result type for this function.  It will be a number of the same
     type as the two inputs.

The way the type constructor for functions (→) is defined makes currying the de-
fault in Haskell. This is because it is an infix operator and right associative.
Because it associates to the right, types are implicitly parenthesized like so:

                 f :: a →  a →  a
Associates to:
                 f :: a → (a → a)
And:
                 map :: (a → b) →  [a] → [b]
Associates into:
                 map :: (a → b) → ([a] → [b])

Let's see if we can unpack  the notion of a right-associating infix operator gi-
ving us curried functions. The association here, or grouping into parentheses is
not to control precedence or order of evaluation:it only serves to group the pa-
rameters into argument and result, since there can only be  one argument and one
result per arrow. Since all the arrows have the same  precedence, the associati-
vity does not change the precedence  or order of evaluation.  Remember,  when we
have a lambda expression that  appears to have two arguments, they  are actually
nested lambadas. Applying the expression to one argument returns a function that
awaits application to a second argument. After you apply it to a second argument
you have a final result. You can nest more lambdas than two, of course,  but the
process is the same: one result, even though that result may be a function awai-
ting application to another argument.
The type constructor for functions and the types we see above are the same thing
but written in  Haskell. When there are "two arguments" in Haskell, we apply our
to an argument, just like when we  apply a lambda expression to an argument, and
then return a result that is a function and  needs to be applied to a second ar-
gument.
Explicit parenthesization, as when an input parameter is itself a function (such
as in map, above), may  used to  indicate order of evaluation, but the  implicit
associativity of the function type doesn't mean the inner or final set of paren-
theses, i.e., the result type, evaluates first. Application is evaluation: in o-
ther words,the only way to evaluate anything is by applying functions, and func-
tion application is left associative.  So, the leftmost, or outermost, arguments
will be evaluated first, assuming anything gets evaluated (sinze Haskell is non-
strict, you can't assume that anything will  be evaluated, but this will be more
clear later).

Partial application
--------------------------------------------------------------------------------
Currying may be interesting, but many people wonder what the practical effect or
value of currying is. We'll look now at a strategy called partial application to
see what currying does for us.It's something we'll explore more as we go through
the book, but we'll start with a relatively uncomplicated example. In this exam-
ple we use the double colon to assign a type. Also, making the type concrete  we
will eliminate the typeclass constraint:

                 addStuff :: Integer → Integer → Integer
                 addStuff a b = a + b + 5

So, addStuff appears to take two Integer arguments and return an Integer result.
But after loading that in GHCi we see that it is taking one  argument and retur-
ning a function that takes one argument and returns one result:

                 Prelude> :t addStuff
                 addStuff :: Integer → Integer → Integer
                 Prelude> let addTen = addStuff 5
                 Prelude> :t addTen
                 addTen :: Integer → Integer
                 Prelude> let fifteen = addTen 5
                 Prelude> fifteen
                 15
                 Prelude> addTen 15
                 25
                 Prelude> addStuff 5 5
                 15

Here fifteen is equal to addStuff 5 5,because addTen is equal to addStuff 5. The
ability to apply only some  of a function's arguments is called partial applica-
tion. This lets us reuse addStuff and  create a new function from it with one of
the arguments applied.If we recall that (→) is a type constructor and associates
to the right, this becomes more clear:

                 addStuff :: Integer →  Integer → Integer

But with explicit parenthesization:

                 addStuff :: Integer → (Integer → Integer)

Applying addStuff to one Integer argument gave us the function addTen, which  is
the return function of addStuff. Applying addTen to an Integer argument gives us
a return value, so the type of fifteen is Integer -no more function arrows.Let's
check our understanding with a function that isn't commutative:

                 subtractStuff :: Integer → Integer → Integer
                 subtractStuff x y = x - y - 10
                    
                 subtractOne = subtractStuff 1

                 Prelude> :t subtractOne
                 subtractOne :: Integer → Integer
                 Prelude> let result = subtractOne 11
                 Prelude> result
                 -20

Why did we get this result?  Because of the order in which we applied arguments,
result is equal to 1 - 11 - 10 = -20.

Manual currying and uncurrying
--------------------------------------------------------------------------------






 
