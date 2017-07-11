Chapter 5: Types
--------------------------------------------------------------------------------
In the last chapter,  we looked at some build-in datatypes, such as Bool and tu-
ples, and we had a brief run-in with the typeclasses Num and Eq. However, a deep
understanding of types and how to read and interpret them is fundamental to rea-
ding and writing Haskell. As we have seen, a datatype declaration defines a type
constructor and data constructor. Data constructors are the values of a particu-
lar type; they are also functions that let us create data, or values,  of a par-
ticular type, although it will take some time before the full import of this be-
comes clear. In Haskell, you cannot create untyped data,  so except for a sprin-
kling of syntactic sugar for things like numbers or functions, everything origi-
nates in a data constructor from some definition of a type.
In this chapter, we are going to take a deeper look at the type system and:

 * learn more about querying and reading type signatures
 * see that currying has, unfortunately, nothing to do with food
 * take a closer look at different kinds of polymorphism
 * look at type inference and how to declare types for our functions

What are types for?
--------------------------------------------------------------------------------
Haskell is an implementation of a pure lambda calculus,  in the sense that it is
not much more than syntactic sugar over the  basic system of variables, abstrac-
tions, and  applications that constitute the rules of  the lambda  calculus  -at
least, of a typed lambda calculus. Developments in logic,  mathematics, and com-
puter science led to the discovery (or invention take you pick) of a typed lamb-
da calculus called "System F" in the 1970s.  Haskell has improved on System F in
some key wats,  such as by allowing general recursion  ( more on that in a later
chapter) and the Hindley-Milner system to permit type inference (more on that on
this chapter), but the core logic is the same.
So, why do we want types? Type systems in logic and mathematics have been desig-
ned to impose constraints that enforce correctness. For our purposes, we can say
that well-designed type systems help eliminate some classes of errors as well as
concerns such as what the effect of a conditional over a non-boolean value might
be.  A type system defines the associations between different parts of a program
and checks that  all the parts fit together in a  logically consistent, provably
correct way.
Let's consider a short, somewhat oversimplified example.  The Bool type is a set
with two inhabitants, True and False, as we saw in the last chapter. Anytime the
value True or False occurs in  a Haskell program, the typechecker will know they
are members of the Bool type. The inverse is that whenever the type Bool is  de-
clared in a type signature, the compiler will expect one of those two values and
only one of those two  values; you get a type error if you try to pass a  number
where a Bool is expected. In Haskell,where typing is static, typechecking occurs
at compile time. That means many errors will be caught before you tru to execute
or run you program. The difference isn't always obvious because GHCi allows  you
to typecheck things interactively, as you are writing them, as well  as  execute
them if they typecheck. No type system can eliminate all possibilities for error
so the possibility of runtime errors and exceptions still exists, and testing(to
which we devote an entire later chapter) of programs is still necessary, but the
type system cuts down on it a great deal.
Good type  systems can also enable compiler optimizations,  because the compiler
can known and predict certain things  about the execution of a  program based on 
the types.  Types can also serve as documentation of your program, which is  why
we encourage  you to declare types (that is, write the type signatures) for your
functions.It will not matter too much when you're writing small programs, but as
programs get longer,type signatures alone can help you read your program and re-
member what you were doing, and help anyone else who might be trying to use your
code as well. If this paragraph didn't make sense to you now, that's fine -we'll
return to these points several times later in the book.
You may feel that Haskell's type system requires a lot of upfront work. This up-
front cost comes with a later payoff: code that is safer and, down the line, ea-
sier to maintain. Working with a good type system can eliminate those tests that
only check that you are passing the right sort of data around,  and since  tests
are more code that you have to write (correctly) and maintain,it will eventually
save time and effort. Many, perhaps most,programming languages have type systems
that feel like haggling with a petty merchant.  However, we believe Haskell pro-
vides a type system  that more closely resembles a quiet,  pleasant conversation 
with a colleague than an argument in the bazaar.Much of what we suggest with re-
gards to putting code in a file, loading it in a REPL,querying types in the REPL
and so forth,is about creating habits conducive to having this pleasant back and
forth with your type systems.

How to read type signatures
--------------------------------------------------------------------------------
In previous chapters, we have seen that we can query types in the REPL by use of 
the ":type" or ":t" command.You can query types for functions, partially applied
functions, and values, which are, in a way, fully applied functions.When we que-
ry the types of values, we see something like this:
                 Prelude> :type 't'
                 't' :: Char              -- 't' has the type Char
                 Prelude> :type "julie"   -- "julie" has the type String
                 "julie" :: [Char]
                 Prelude> :type True      -- True has the type Bool
                 True :: Bool

When we query the types of numeric values,  we see typeclass information instead
of a concrete type,  because the  compiler does not know  which specific numeric
type a value is until the  type is either declared or the compiler  is forced to
infer a specific type based on the function.For example, 13 may look like an in-
teger to us,but that would only allow us to use it in computations that take in-
tegers (and not in fractional division).   For that reason, the  compiler  gives
in the type with the broadest applicability (most polymorphic) and says it is a
constrained polymorphic " Num a ⇒ a " value:
                 Prelude> :type 13
                 13 :: Num a => a
But we can give it a concrete type by declaring it:
                 Prelude> let x = 13 :: Integer
                 Prelude> :t x
                 x :: Integer

Polymorphism, polymorphic numerals, and type inference will be addressed in more
detail later. You can also query the type signatures of functions, as we've seen
                 Prelude> :type not
                 not :: Bool → Bool
This takes one input of a Bool value and returns one Bool value. Given that type
there aren't too many things it even could do.

Understanding the function type
--------------------------------------------------------------------------------
The arrow, → , is the type constructor for functions in Haskell. It's baked into
the language, but syntactically it works in very much the same way as all the o-
ther types you've seen so far. It's a type constructor, like Bool,  except the →
type constructor takes arguments and has no data constructors:
                 Prelude> :info (->)
                 data (->) a b
If you compare this to the type constructor of the two-tuple,you see the simila-
rity:
                 Prelude> :info (,)
                 data (,) a b = (,) a b
We saw earlier that  the tuple constructor needs to be applied to two  values in
order to construct a tuple.A function must similarly have two arguments -one in-
put and one result-  in order to  be a function.  Unlike the tuple  constructor,
though, the function type  has no data constructor.  The value that shows  up at
term level is the function. Functions are values. As we've said, the hallmark of
a function is that it can be applied, and the structure of the type demonstrates
this.  The arrow is an infic operator that has two  parameters and associates to
the right (although function application is left associative). The parameteriza-
tion suggests that we'll apply the  function to some argument that will be bound
to the first parameter, with the second parameter, b, representing the return of
result type. We will cover these things in more detail throughout this chapter.
Let's return to reading type signatures. The function fst is a value of the type
"(a,b) → a " where → is an infix type constructor that takes two arguments.
                 fst :: (a,b)  →   a
                         [1]  [2] [3]
[1]. The first parameter of fst has the type (a,b). Note that the tuple type it-
     self (,) takes two arguments a and b here.
[2]. The function type, (→), has two parameters here.One is (a,b) and one is the
     result a.
[3]. The result of the function, which has type a. It is the same a  that was in
     the tuple (a,b).

How do we know it is the same a?  As we learn more about type variables, it  will
become more clear how we know.  We can say that we know the input  type a and the
output type a must be the same type, and we can see that  nothing happens between
the input and the output; that is, there is no operation  that comes between them
that could transform that a into some other value of that type.We'll go into more
detail about those points later in the chapter. Let's look at another function:
                 Prelude> :type length
                 length :: [a] → Int
The length function takes one argument that is a list -note the square brackets-
and returns and Int result. The Int result in this case will be the number of i-
tems in the list.  The type of the  inhabitants of the list is left unspecified;
this function does not care, in fact cannot care,what types of values are inside
the list.

Typeclass-constrained type variables
--------------------------------------------------------------------------------
Next, let's look at the types of some arithmetic functions.  You may recall that
the act of wrapping an  infic operator in parentheses allows us to use the func-
tion just like a normal prefix function, including being able to query the type:
                 Prelude> :type (+)
                 (+) :: Num a ⇒ a → a → a
                 Prelude> :type (/)
                 (/) :: Fractional a ⇒ a → a → a
To describe these casually,we could say addition takes one numeric argument adds
it to a second numeric argument of the same type, and returns a numeric value of
the same type as a result.  Similarly,  the fractional division function takes a
fractional  value, divides it by a second fractional  value, and returns a third
fractional value as a result.  This is not precise, but it will do for now.  The
compiler gives the least specific and most general type it can. Instead of limi-
ting this  function to a concrete type,  we get a typeclass-constrained polymor-
phic type variable.  We'll save a fuller explanation of typeclasses for the next
chapter. What we need to know here is that each typeclass offers standard set of
functions that  can be used across several concrete  types. When a  typeclass is
constraining  a type variable  in this way,  the variable could represent any of
the concrete types that have instances of that typeclass so that specific opera-
tions on which the function depends are defined for that type. We say it's cons-
trained because we still do not know  the concrete type  of a, but we do know it 
can only be one of the types that has the required typeclass instance.
This generalization of number-hood is what lets  us use the same numerical lite-
rals to represent numeric values of different types.
We can start with a " Num a ⇒ a " value and then create specific versions of it
with a concrete type using the :: to assign a type to the value:
                 Prelude> let fifteen = 15
                 Prelude> :t fifteen
                 fifteen :: Num a ⇒ a
                
                 Prelude> let fifteenInt    = fifteen :: Int
                 Prelude> let fifteenDouble = fifteen :: Double

                 Prelude> :t fifteenInt
                 fifteenInt :: Int

                 Prelude> :t fifteenDouble
                 fifteenDouble :: Double
We went from "Num a ⇒ a" to Int and Double.  This works because  Int and Double
each have an instance of the Num typeclass:
                 Prelude> :info Num
                 ...
                 instance Num Int    -- Defined in ´GHC.Num´
                 instance Num Double -- Defined in ´GHC.Float´
Since they both have instances of Num, the operations from Num, such as addition
are defined for both of them:
                 Prelude> fifteenInt + fifteenInt
                 30
                 Prelude> fifteenDouble + fifteenDouble
                 30.0
We can also make more specific  versions of our "Num a ⇒ a" value named  fifteen
by using it in a way that requires it to become something more specific:
                 Prelude> fifteenDouble + fifteen
                 30.0
                 Prelude> fifteenInt + fifteen
                 30
However, this will not work:
                 Prelude> fifteenDouble + fifteenInt
                 Couldn't match expected type `Double` with actual type `Int`.  
                 In the second argument of ´(+)´, namely ´fifteenInt´
                 In the expression: fifteenDouble + fifteenInt
We can't add those two values because their types are no longer polymorphic, and
their concrete types are  different so they have different definitions of how to
implement addition.  The type error messages contrasts  the actual type with the 
expected type. The actual type is what we provided;the expected type is what the
compiler expected. Since we had fifteenDouble as our first argument, it expected
the second value to also have the type Double but it actually has the type Int.
A type signature might have multiple typeclass constraints on one or more of the
variables. You will sometimes see (or write) type signatures such as:
                 (Num a, Num b) ⇒ a → b → b
                 -- or
                 (Ord a, Num a) ⇒ a → a → Ordering
Here,the constraints look like a tuple but they don't add another function argu-
ment that you must provide,and they don't appear as a tuple at the value of term
level. Nothing to the  left of the typeclass  arrow, ⇒, shows up at term level. 
The tuple of constraints does represent a product or conjunction of constraints.
In the first example above, there  are two constraints, one  for each  variable. 
Both a and b must have instances of the Num typeclass.In the second example,both
of the constraints are on the one variable a -that is, a must be a type that im-
plements both Ord and Num.

Exercises: Type Matching
--------------------------------------------------------------------------------
Below you'll find a list of several standard functions we have talked about pre-
viously. Under that is a list of their type signatures.Match the function to its
type signature.  Try to do it without peeking at the type signatures  (either in 
the text or in GHCi)  and then check your work.  You may find it easier to start
from the types and work out what you think a function of that type would do.
1. Functions:
   a. not
      not :: Bool → Bool
   b. length
      length :: [a] → Int
   c. concat
      concat :: [[a]] → [a]
   c. head
      head :: [a] → a
   d. (<)
      (<) :: Ord a ⇒ a → a → Bool

