Type Inference
--------------------------------------------------------------------------------
Haskell does not obligate us to  assert a type for every  expression or value in
our programs because it has type  inference.  Type inference is an algorithm for
determining the types of expressions.Haskell's type inference is built on an ex-
tended version of the Damas-Hindley-Milner type system.   Haskell will infer the
most generally applicable (polymorphic) type that is still concrete. Essentially
the compiler starts from the values whose types it  knows and then works out the
types of the other values.  As you mature as a Haskell programmer, you will find
this is principally useful for when you are sill  figuring out  new code  rather 
than for code  that is "done".  Once your program is "done", you will  certainly
know the types of all the functions, and it's considered good practice to expli-
citly declare them.  Remember when we suggested that a good type system was like
a pleasant conversation with a college? Think of the type inference as a helpful
colleague working through a problem with you.  For example, we can write id our-
selves:

                 Prelude> let ourId x = x
                 Prelude> :t ourId
                 ourId :: t → t
                 Prelude> ourId 1
                 1
                 Prelude> ourId "blah"
                 "blah"

Here we let GHCi infer the type of ourId itself.  Due to alpha equivalence,  the
difference in letters (t here versus a above) makes no difference.Type variables
have no meaning  outside of the  type signatures  where they are bound. For this 
function, we again ask the compiler to infer the type:

                 Prelude> let myGreet x = x ++ " Julie"
                 Prelude> myGreet = "hello"
                 "hello Julie"
                 Prelude> :t myGreet
                 myGreet :: [Char] → [Char]

The compiler knows the function (++) and has one value to work with already that
it knows is a String.It doesn't have to work very hard to infer a type signature
from that information.  If, however, we take out the string value and replace it
with another variable, see what happens:

                 Prelude> let myGreet x y = x ++ y
                 Prelude> :t myGreet
                 myGreet :: [a] → [a] → [a]

We're back to a polymorphic type signature, the same signature for (++) itself,
because the compiler  has no information by which to infer the types for any of
those variables  (other than  that they are lists of some sort). Let's see type
inference at work:

> f :: Num a => a -> a -> a
> f x y = x + y + 3

If we load this function into GHCi to experiment:

                 Prelude> :l 055-TypeInference.lhs
                 [1 of 1] Compiling 055-TypeInference
                 Ok, modules loaded: 055-TypeInference,
   
                 Prelude> f 1 2
                 6
                 Prelude> :t f
                 f :: Num a ⇒ a → a → a
                 Prelude> :t f 1
                 f 1 :: Num a ⇒ a → a

Because the numeric literals in Haskell have the (typeclass constrained)polymor-
phic type "Num a ⇒ a", we do not get a more specific type when applying f to  1.
Look at what happens when we elide the explicit type signature for g:

> g x y = x + y + 3

No type signature for g, so does it compile? Does it work?

                 Prelude> :l 055-TypeInference.lhs
                 [1 of 1] Compiling 055-TypeInference
                 Ok, modules loaded: 055-TypeInference.
        
                 Prelude> :t g
                 g :: Num a ⇒ a → a → a
                 Prelude> g 1 2
                 6

Nothing changes.  In certain cases there migth be a change, usually when you are
using typeclasses in a way that doesn't make it clear which type you mean unless
you assert one.

Exercises: Apply Yourself
--------------------------------------------------------------------------------
Look at these pairs of functions. One function is unapplied,so the compiler will
infer maximally polymorphic type.The second function has been applied to a value
so the inferred type signature may have become concrete,  or at least less poly-
morphic.  Figure out how the type would change and why,  make a note of what you 
think the new inferred type would be and then check your work in GHCi.

1. Type signature of general function:

                 (++) :: [a] → [a] → [a]

   How might that change when we apply it to the following value?

                 myConcat x = x ++ " yo"


2. Type signature of general function:

                 (*) :: Num a ⇒ a → a → a

   Applied to a value:
 
                 myMult x = ( x / 3 ) * 5


3. General:
          
                 take :: Int → [a] → [a]

   Applied to:
 
                 myTake x  = take x "hey you"


4. General:
                 (>) :: Ord a ⇒ a → a → Bool

   Applied to:

                 myCom x = x > (length [1..10])


5. General:
      
                 (<) :: Ord a ⇒ a → a → Bool

   Applied to:

                 myAlph x = x < 'z'



Asserting types for declarations
--------------------------------------------------------------------------------
Most of the time, we want to declare our types,rather than relying on type infe-
rence.  Adding type  signatures to your code  can provide guidance to you as you 
write your functions.  It can also help the compiler  give you information about
where your code is going wrong. As programs become longer and more complex, type
signatures become even  more important,  as they help  you or other programmiers
trying to use your code read it  and figure out what it is supposed to do.  This
section will look at how to  declare types. We will start with some tivial exam-
ples. You may remember the triple function we have seen before.  If we allow the
compiler to infer the type, we end up with this:

                 Prelude> let triple x = x * 3
                 Prelude> :t triple
                 triple :: Num a ⇒ a → a

Here the triple function was made from the  (*)  function with has the following
type:  "(*) :: Num a ⇒ a → a → a",  but we have already applied one of the argu-
ments, which is the 3, so there is one less parameter in this type signature. It
is still polymorphic because it can not tell what type 3 is yet. If, however, we
want to ensure  that our inputs and result  may only by integer,  this is how we
declare that:

                 Prelude> let triple x = x * 3 :: Integer
                 Prelude> :t triple
                 triple :: Integer → Integer

Note that the typeclass constraint is gone,because Integer implements Num,making
that constraint redundant. Here's another example of a type declaration for  our
triple function, this one more like what you would see in a source file:

> triplex :: Integer -> Integer
> triplex x = x * 3

This is how most Haskell code you look at will be laid out,with separate top le-
vel declarations  for types and functions.  Such top  level declarations  are in
scope throughout the module. It is possible,  though uncommon, to declare  types
locally with let and where clauses. Here's an example of assigning a type within
a where clause: 

> triples x = tripleIt x
>   where
>     tripleIt :: Integer -> Integer
>     tripleIt y = y * 3

The assertion in the where clause narrowed our type down from:
                 Num a ⇒ a → a
To:
                 Integer → Integer

GHCi will pick up and propagate type information for inference from applications
of functions, sub-expressions, definitions -almost anywhere.  The type inference
is strong with this one.  There are constraints on our ability to declare types.
For example, if we try to make the (+) function return a String, we get an error
message:

                 Prelude> let x = 5 + 5 :: String

                 No instance for (Num String) arising from a use of `+`
                 In the expression: 5 + 5 :: String
                 In an equation for 'x' : x = 5 + 5 :: String

This function cannot accept arguments of type String. In this case it is overde-
terminated, both because the  (+) function is limited to types  implementing the
Num typeclass and also because we have already passed it two numeric literals as
values.The numeric literals could be any of several numeric types under the hood
but they can't be String because String does not implement the Num typeclass.
-- 147
