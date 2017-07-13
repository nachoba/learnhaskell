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
nested lambdas.  Applying the expression to one argument returns a function that
awaits application to a second argument. After you apply it to a second argument
you have a final result. You can nest more lambdas than two, of course,  but the
process is the same: one result, even though that result may be a function awai-
ting application to another argument.
The type constructor for functions and the types we see above are the same thing
but written in  Haskell. When there are "two arguments" in Haskell, we apply our
function to an argument,just like when we  apply a lambda expression to an argu-
ment, and then return a result that is a function and  needs to be applied to  a
second argument.
Explicit parenthesization, as when an input parameter is itself a function (such
as in map, above), may be used to indicate order of evaluation, but the implicit
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
Haskell is curried by default,but you can uncurry functions.Uncurrying means un-
nesting the functions and replacing the two functions  with a tuple of two vales
(these would be the two values you want to use as arguments). If you uncurry (+)
the type changes from "Num a ⇒ a → a → a" to Num a ⇒ (a,a) → a which better fits
the description "takes two arguments,returns one result" than curried functions.
Some older functional languages  default to using a product type  like tuples to 
express multiple arguments.

                 * Uncurried functions : One function, many arguments.
                 * Curried functions   : Many functions, one argument apiece

You can also de-sugar  the automatic currying yourself, by nesting the arguments
with lambdas, though there's almost never a reason to do so. We'll use anonymous
lambda syntax here to show you  some examples of uncurrying. You may want to re-
view anonymous lambda syntax or try comparing these functions directly and thin-
king of the backslash as lambda:

> nonsense :: Bool -> Integer
> nonsense True  = 805
> nonsense False = 31337
>
> curriedFunction :: Integer -> Bool -> Integer
> curriedFunction i b = i + (nonsense b)
>
> uncurriedFunction :: (Integer, Bool) -> Integer
> uncurriedFunction (i,b) = i + (nonsense b)
>
> anonymous :: Integer -> Bool -> Integer
> anonymous = \i b -> i + (nonsense b)
>
> anonNested :: Integer -> Bool -> Integer
> anonNested = \i -> \b -> i + (nonsense b)

When we test these functions in the REPL, we get:

                 Prelude> curriedFunction 10 False
                 31347
                 Prelude> anonymous 10 False
                 31347
                 Prelude> anonNested 10 False
                 31347
                 
They are all the same function,  all giving the same results.  In anonNested, we
manually  nested the  anonymous lambda to  get a function that was  semantically
identical to curriedFunction but did not leverage the  automatic currying.  This
means functions that seem to accept multiple arguments such as with  "a → a → a"
are higher-order  functions: they yield more function values as each argument is
applied until there are no more (→) type constructors and it terminates in a non
function value.  

Currying and uncurrying existing functions
--------------------------------------------------------------------------------
It turns out,we can curry and uncurry functions with multiple parameters generi-
cally without writing  new code for each one. Consider the following example for
currying:

                 Prelude> let curry f a b = f (a , b)
                 Prelude> :t curry
                 curry :: ((t1,t2) → t) → t1 → t2 → t
                 Prelude> :t fst
                 fst :: (a, b) → a
                 Prelude> :t curry fst
                 curry fst :: t → b → t
                 Prelude> fst (1,2)
                 1
                 Prelude> curry fst 1 2
                 1

> curry' :: ((t1,t2) -> t) -> t1 -> t2 -> t
> curry' f a b = f (a,b)

Then for uncurrying:

                 Prelude> let uncurry f (a,b) = f a b
                 Prelude> :t uncurry
                 uncurry :: (t1 → t2 → t) → (t1, t2) → t
                 Prelude> :t (+)
                 (+) :: Num a ⇒ a → a → a
                 Prelude> (+) 1 2
                 3
                 Prelude> uncurry (+) (1,2)
                 3

> uncurry' :: (t1 -> t2 -> t) -> (t1,t2) -> t
> uncurry' f (a,b) = f a b

Currying  and uncurrying  functions of three or  more arguments automatically is
quite possible, but tricker. We'll leave that be, but investigate on your own if
you like.

Sectioning
--------------------------------------------------------------------------------
We mentioned sectioning before, and now that we've  talked a bit more about cur-
rying and partial application, it should be more clear what is happening  there.
The term  sectioning  specifically refers to partial application of infix opera-
tors, which has a special  syntax and allows you to choose  whether the argument
you're partially applying the operator to is the first or second argument:
     
                 Prelude> let x = 5
                 Prelude> let y = (2^)
                 Prelude> let z = (^2)
                 Prelude> y x
                 32
                 Prelude> z x
                 25

With commutative functions such as addition, the argument order does not matter.
We will usually section addition as, for example, (+3), but later  when we start
using partially applied functions a lot with maps and folds and so forth, you'll
be able to see the difference that the argument  order can make with noncommuta-
tive operators. This does not only work with arithmetic, though:

                 Prelude> let celebrate = ( ++ " woot!")
                 Prelude> celebrate "naptime"
                 "naptime woot!"
                 Prelude> celebrate "dogs"
                 "dogs woot!"

You can also use the syntax with  functions that are normally  prefix if you use
the backticks to make  them infix ( note the .. is shorthand for  constructing a
list of all elements between the first and last values given -go ahead and  play
with this in your REPL):

                 Prelude> elem 9 [1..10]
                 True
                 Prelude> 9 `elem` [1..10]
                 True

                 Prelude> let c = (`elem` [1..10])
                 Prelude> c 9
                 True

                 Prelude> c 25
                 False

If you partially applied  elem in its usual prefix  form, then the argument  you
apply it to would necessarily be the first argument:

                 Prelude> let hasTen = elem 10
                 Prelude> hasTen [1..9]
                 False
                 Prelude> hasTen [5..15]
                 True

Partial application is common enough in Haskell that, over time,  you'll develop
an intuition for it. The sectioning syntax exists to allow some freedom in which
argument of a binary operator you apply the function to.

Exercises: Type Arguments
--------------------------------------------------------------------------------
Given a function and its type, tell  us what type results from  applying some of
all the arguments.You can check your work in the REPL like this (using the first
question as an example):

> f :: a -> a -> a -> a
> f = undefined
>
> x :: Char
> x = undefined

It turns out that you can check the types of things that aren't really implemen-
ted yet, so long as you give GHCi and undefined to bind the signature to.
1. If the type of f is "a → a → a → a", and the type of x is Char then the  type
   of x is:
   a) Char → Char → Char

2. If the type of g is "a → b → c → b", then the type of: g 0 'c' "woot" is:

> g :: a -> b -> c -> b
> g = undefined

   d) Char

3. If the type of h is: "h :: (Num a, num b) ⇒ a → b → b", then the type of:
   h 1.0 2 is:

> h :: (Num a, Num b) => a -> b -> b
> h = undefined

   a) Num b ⇒ b 
   Note that because the type variables a and b are different,the compiler must
   assume that the types could be different.

4. If the type of h is: "h :: (Num a, Num b) ⇒ a → b → b", then the type of:
   h 1 (5.5 :: Double) is:

> h' :: (Num a, Num b) => a -> b -> b
> h' = undefined

  c) Double

5. If the type of "jackal :: (Ord a, Eq b) ⇒ a → b → a", then the type of:
   jackal "keyboard" "has the word jackal in it"

> jackal :: (Ord a, Eq b) => a -> b -> a
> jackal = undefined

   a) [Char]

6. If the type of "jackal' :: (Ord a, Eq b) ⇒ a → b → a", then the type of:
   jackal' "keyboard"

   e) Eq b ⇒ b → [Char]

7. If the type of "kessel :: (Ord a, Num b) ⇒ a → b → a", then the type of:
   kessel 1 2 is:

> kessel :: (Ord a, Num b) => a -> b -> a
> kessel = undefined

   f) (Num a, Ord a) ⇒ a

8. Using kessel, what is the type of: "kessel 1 (2 :: Integer)"
   e) (Num a, Ord a) ⇒ a

9. Using kessel, what is the type of: "kessel (1 :: Integer) 2"
   c) Integer




 
