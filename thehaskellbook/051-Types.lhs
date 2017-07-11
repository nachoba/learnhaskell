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

