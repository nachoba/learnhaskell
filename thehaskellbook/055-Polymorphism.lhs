Polymorphism
--------------------------------------------------------------------------------
Polymorphic type variables give us the ability to implement expressions that can
accept arguments and return  results of different  types without having to write
variations on  the same expression for each type. It would be inefficient if you
were doing arithmetic and had to write the same code over and over for different
numeric types. The good news is the numerical functions that come with  your GHC
installation and the  Prelude are polymorphic by default. Broadly speaking, type
signatures  may have three kinds of types: concrete, constrained polymorphic, or
parametrically polymorphic.
In Haskell,polymorphism divides into two categories: parametric polymorphism and
constrained polymorphism. If you've encountered polymorphism before, it was pro-
bably a form of constrained, often called ad-hoc,  polymorphism. Ad-hoc polymor-
phism in Haskell is emplemented with typeclasses.
[See Wadler:  http://people.csail.mit.edu/dnj/teaching/6898/papers/wadler88.pdf]
Parametric polymorphism is broader than ad-hoc polymorphism. Parametric polymor-
phism refers to type variables, or parameters, that are fully polymorphic.  When
unconstrained by a typeclass, their final, concrete type could be anything.Cons-
trained polymorphism, on the other hand,puts typeclass constraints on the varia-
ble, decreasing the number of concrete types it could be,but increasing what you
can actually do with it by defining and bringing into scope a set of operations.
Recall  that when you see a lowercase name in a type signature, it is a type va-
riable and polymorphic (like a, t, etc). If the type is capitalized,it is a spe-
cific, concrete type such as Int, Bool, etc.
Let's consider a parametrically polymorphic function: identity.  The id function
comes with the Haskell Prelude and is called the identity function because it is
the identity for any value in our language. In the next example, the type varia-
ble 'a' is parametrically polymorphic and not constrained by a typeclass.Passing
any value to id will return the same value:

                 id :: a → a

For all 'a',get an argument of some type 'a' and return a value of the same type
'a'. This is the maximally polymorphic signature for id. It allows this function
to work with any type of data:

                 Prelude> id 1
                 1
                 Prelude> id "blah"
                 "blah"
                 Prelude> let inc = (+1)
                 Prelude> inc 2
                 3
                 Prelude> (id inc) 2
                 3

Based on the type of id, we are guarantedd this behavior -it cannot  do anything
else!. The a in the type signature cannot change because the  type variable gets
fixed to a concrete type throufhout the entire type signature  (a == aa). If one
applies id to a value of type Int, the a is fixed to type Int.  By default, type
variables are resolved at the left-most part of the type signature and are fixed
once sufficient information to bind them to a concrete type is available.The ar-
guments in parametrically polymorphic functions, like id, could be anything, any
type or typeclass,so the terms of the function are more restricted because there
are no methods or information attached to them. With the type "id :: a → a" , it
can do nothing other than return a because there is no information or method at-
tached to its parameter at all -nothing can be done with a. On the other hand, a
function like negate, with a similar-appearing type signature of "Num a ⇒ a → a"
constrains  the a variable as an  instance of the Num typeclass. Now a has fewer
concrete  types it could be, but there is a set of methods you can use, a set of
things that can be done with a.
If a variable  represents a set  of possible values, then a type variable repre-
sents a set of possible types. When there is no typeclass constraint, the set of
possible  types a variable  could represent is effectively unlimited.  Typeclass
constrains  limit the set of potential types (and, thus, potential values) while
also passing along the common functions that can be used with those values. Con-
crete types have even more flexibility in  terms of computation.  This has to do
with the additive nature of typeclasses. For example, an Int is only an Int, but
it can make use of  the methods of  the Num and  Integral typeclasses because it 
has instances of both.  We can describe Num as a superclass of several other nu-
meric typeclasses that all inherit operations from Num.
In sum, if a variable could be anything, then there's little that can be done to
it because it has no methods.If it can be some types (say,a type that is an ins-
tance of Num), then it has some  methods. If it is a concrete type, you lose the
type flexibility but,  due to the additive nature of typeclass inheritance, gain 
more potential methods.It's important to note that his inheritance extends down-
wards from a superclass, such as Num to sublcasses such as Integral and then Int
but not the other way around.That is, if something is an instance of Num but not
an instance of Integral, it cannot  implement the methods of the  Integral type-
class.  A subclass  cannot override the methods of its superclass. A function is
polymorphic  when its type signature has  variables that can represent more than
one type. That is, its parameters are polymorphic.Parametric polymorphism refers
to fully polymorphic (unconstrained by a typeclass) parameters. Parametricity is
the property we get from having parametric polymorphism.Parametricity means that
the behavior of a function with respect to the types of its(parametrically poly-
morphic) arguments is  uniform.  The behavior can not change just because it was 
applied to an argument of different type.

Exercises: Parametricity
--------------------------------------------------------------------------------
All you  can really do  with a  parametrically  polymorphic value is pass or not
pass it to some other expression.Prove that to yourself with these small demons-
trations.

1. Given the type "a → a", which is the type for id, attempt to make a function
   that  is not  bottom and  terminates  successfully that does something other
   than returning the same value.This is impossible, but you should try it any-
   way.

2. We can  get a more  comfortable  appreciation of parametricity  by looking at
   "a → a → a". This hypothecial function has two and only  two-implementations.
   Write both possible versions of it. After doing so, try  to violate the cons-
   traints of parametrically polymorphic values we outlined above. 

3. Implement "a → b → b".How many implementations can it have? Does the behavior
   change when the types of a and b change?

Polymorphic Constants
--------------------------------------------------------------------------------
We've seen that there are several types of numbers in Haskell and that there are
restrictions on using different types of numbers in different functions, But in-
tuituvely we see it would be odd  if we could  not do arithmetic along the lines
of -10 + 6.3. Well, let's try it:

                 Prelude> :t (-10) + 6.3
                 (-10) + 6.3 :: Fractional a ⇒ a
                 Prelude> :t (-10)
                 (-10) :: Num a ⇒ a

page 141

