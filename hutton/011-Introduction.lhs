Introduction
--------------------------------------------------------------------------------
Chapter 1 - Programming in Haskell by Graham Hutton

Functions
--------------------------------------------------------------------------------
In Haskell,a function is a mapping that takes one or more arguments and produces
a single result,and is defined using an equation that gives a name for the func-
tion, a name for each of its arguments, and a body that specifies how the result
can be calculated in terms of the arguments. For example, a function double that
takes a number x as its argument, and produces the result x + x,  can be defined
by the following equation:

> double x = x + x

When a function is applied to actual arguments,the result is obtained by substi-
tuting these arguments  into the body of  the function in place  of the argument
names. This process may immediately produce a result that cannot be further sim-
plified, such as a number.

More commonly, the result will be an expression containing other function appli-
cations, which must then be  processed in the same way to produce the  final re-
sult. For example, the result of the application of  "double 3" of the function
double to the number 3 can be determined by the following calculation, in which
each step is explained by a short comment.

    double 3
=      {applying double}
    3 + 3
=      {applying +}
    6

In general, the order in which functions are  applied in a calculation  does not
affect the value of the final result, but  it may affect the number of steps re-
quired, and whether the calculation process terminates.

Functional programming
--------------------------------------------------------------------------------
What is functional programming? Generally speaking,functional programming can be
viewed as a style of programming in which the basic method of computation is the
application of functions to arguments. In turn,a functional programming language
is on that supports and encourages the functional style.

Features of Haskell
--------------------------------------------------------------------------------
The main features of Haskell are listed below:
* Concise programs       : Due to the high-level nature of the functional style,
                           programs  written in Haskell are often much more con-
                           cise than programs  written in other languages. More-
                           over, the syntax of  Haskell has  been designed  with
                           concise programs in mind, in particular by having few
                           keywords, and  by allowing indentation to be  used to
                           indicate the structure of programs.
* Powerful type system   : Most modern  programming  languages include some form
                           of type system to detect incompatibility errors, such
                           as erroneously attempting to  add a number and a cha-
                           racter.  Haskell  has a type system that  usually re-
                           quires little type information  from the  programmer,
                           but allows a large class of incompatibility errors in
                           programs to be automatically detected  prior to their
                           execution, using a sophisticated process called  type
                           inference.The Haskell type system is also more power-
                           ful than most languages,supporting very general forms
                           of polymorphism and overloading, and providing a wide
                           range of special purpose features concerning types.
* List comprehensions    : One  of the most common ways to structure and manipu-
                           late data in computing  is using list values. To this
                           end, Haskell provides lists a a  basic concept in the
                           language, together with a simple but powerful compre-
                           hension notation that  constructs new lists by selec-
                           ting and filtering elements from one of more existing
                           lists.Using comprehension notation allows many common
                           functions on lists to be defined  in a clear and con-
                           cise manner, without the need for explicit recursion.
* Recursive functions    : Most programs involve some form of looping.In Haskell
                           the basic  mechanism by which  looping is achieved is
                           through recursive functions that are defined in terms
                           of themselves.  It can take some time to  get used to
                           recursion,but many computations have a simple and na-
                           tural definition in terms of recursive functions, es-
                           pecially when pattern matching and guards are used to
                           separate different cases into different equations.
* Higher-order functions : Haskell is a higher-order functional language,  which
                           means that  functions can freely  take  functions  as
                           arguments and  produce  functions as  results.  Using
                           higher-order functions allows common programming pat-
                           terns, such as composing two functions, to be defined
                           as functions within the language itself. More genera-
                           lly, higher-order functions can be used to define do-
                           main-specific languages within Haskell itself,such as
                           for list processing,interactive programming, and par-
                           sing.
* Effectful functions    : Functions in Haskell are pure functions that take all
                           their inputs as  arguments and produce all their out-
                           puts as results. However, many programs require  some
                           form of side effect that would appear  to be  at odds
                           with purity, such as reading input from the keyboard,
                           or writing output to the screen, while the program is
                           running.Haskell provides a uniform framework for pro-
                           gramming with effects, without compromising the puri-
                           ty of functions, based upon the use of monads and ap-
                           plicatives.
* Generic functions      : Most languages allow functions to be defined that are
                           generic  over a range of simple types, such as diffe-
                           rent forms of numbers. However, the Haskell type sys-
                           tem also  supports  functions  that are  generic over
                           much richer kinds of structures.For example, the lan-
                           guage provides a range of library  functions that can
                           be used with any type that is functorial, applicative
                           ,monadic, foldable, or traversable, and moreover, al-
                           lows new structures and  generic functions  over them
                           to be defined.
* Lazy evaluation        : Haskell programs are  executed using a technique cal-
                           led lazy evaluation,which is based upon the idea that
                           no computation  should be performed  until its result
                           is actually required. As well as avoiding unnecessary
                           computation, lazy  evaluation  ensures  that programs
                           terminate whenever possible,encourages programming in
                           a modular style  using  intermediate  data structures
                           and even allows programming with infinite structures.
* Equational reasoning   : Because programs in Haskell are pure functions,simple
                           equational  reasoning techniques  can be used to exe-
                           cute programs,to transform programs, to prove proper-
                           ties of programs, and  even to calculate programs di-
                           rectly from  specifications  of their intended  beha-
                           viour.  Equational reasoning is particularly powerful
                           when combined  with the use of induction to reason a-
                           bout functions that are defined using recursion.

Historical background
--------------------------------------------------------------------------------
Many of the features  of Haskell are not new, but were first introduced by other
languages. To help place Haskell in context, some of the key historical develop-
ments related to the language are briefly summarised below:

* In the 1930s, Alonzo Church developed the lambda calculus, a simple but power-
  ful mathematical theory of functions.
* In the 1950s, John McCarthy developed Lips ("LISt Procesor"), generally regar-
  ded as being the first functional programming language. Lisp had some influen-
  ces from the lambda calculus, but still retained  the concept of variable  as-
  signment as a central feature of the language.
* In  the 1960s, Peter Landing  developed ISWIM ("If you See What I Mean"),  the
  first pure functional programming language,based strongly on the lambda calcu-
  lus and having no variable assignments.
* In the 1970s,John Backus developed FP ("Functional Programming"), a functional
  programming  language that  particularly  emphasised the  idea of higher-order
  functions and reasoning about programs.
* Also in the 1970s, Robin Milner and others developed ML ("Meta Language"), the
  first of the modern functional programming languages,which introduced the idea
  of polymorphic types and type inference.
* In the 1970s and 1980s,David Turner developed a number of lazy functional pro-
  gramming languages, culminating in the commercially  produced language Miranda
  (meaning "admirable").
* In 1987,an international committee of programming language researchers initia-
  ted  the development of  Haskell (named after the logician  Haskell Curry),  a
  standard lazy functional programming language.
* In the 1990s,Philip Wadler and others developed the concept of type classes to
  support overloading, and the use  of monads to handle effects, two of the main
  innovative features of Haskell.
* In 2003, the Haskell  committee published  the Haskell Report, which defined a
  long-awaited stable version of the language.
* In 2010, a revised and updated of the Haskell Report was published. Since then
  the language has continued to evolve, in response to both new foundational dev-
  elopments and new practical experience.

A taste of Haskell
--------------------------------------------------------------------------------
We conclude this chapter with three small examples that give a taste of program-
ming in Haskell.  The examples involve  processing lists  of values of different
types, and illustrate different features of the language.

Summing numbers
--------------------------------------------------------------------------------
In Haskell a function sum which produces the sum of a list of numbers can be de-
fined using two equations:

> suma []     = 0
> suma (n:ns) = n + suma ns

The first equation states that the sum  of the empty list is zero, while the se-
cond states that the sum of any non-empty list comprising a first number n and a
remaining list of numbers ns is given by adding n and the sum of ns.Note that e-
ven  though the function suma  is defined in terms of itself and is hence recur-
sive, it does not loop forever. Each application of suma reduces  the length  of
the argument list by one,until the list eventually becomes empty, at which point
the recursion stops and the additions are performed. 

In Haskell every function has a type that specifies the  nature of its arguments
and results,which is automatically inferred from the definition of the function.
For example, the function suma defined above has the following type: 
                                                                 Num a ⇒ [a] → a

This type states that for any type a of numbers, suma is a function  that maps a
list of such numbers  to a single such  number. Types provide useful information
about the nature of functions, but, more  importantly, their use  allows many e-
rrors in programs to be  automatically detected  prior to executing the programs
themselves.

Sorting values
--------------------------------------------------------------------------------
Now let us consider a more sophisticated function concerning lists, which illus-
trates a number of  other aspects of Haskell. Suppose that  we define a function
called qsort by the following two equations:

> qsort     [] = []
> qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
>                 where
>                   smaller = [a | a <- xs, a <= x]
>                   larger  = [b | b <- xs, b >  x]

In this definition ++ is an operator that appends two lists together;for example
[1,2,3] ++ [4,5] = [1,2,3,4,5]. In turn,where is a keyword that introduces local
definitions, in this case a list smaller comprising all elements a from the list
xs that are less than or equal to x, together with a list larger comprising  all
elements b from xs that are greater than x. In summary, qsort has  sorted a list
into numerical order. This method of sorting is called quicksort, and is  one of
the best such  methods knows. The above implementation of quicksort is an excel-
lent example of the power of Haskell,being both clear and concise. Moreover, the
function qsort is also more general than might be expected, being applicable not
just with numbers, but with any type of ordered values. More precisely, the type
"qsort :: Ord a ⇒ [a] → [a]" states that, for any type a of ordered values,qsort
is a function that maps between lists of such values. 

Sequencing actions
--------------------------------------------------------------------------------
Our final example further emphasises  the level of precision and generality that
can be achieved in Haskell. Consider a function called seqn that takes a list of
input/output actions,such as reading or writing a single character,performs each
of these actions in sequence, and returns a list of resulting values.In Haskell,
this function can be defined as follows:

> seqn []          = return []
> seqn (act:acts)  = do
>                     x  <- act
>                     xs <- acts
>                     return (x:xs)

These  two equations  state that if the  list of actions  is empty we return the
empty list of  results, otherwise we  perform the first action in the list, then
perform the remaining actions, and finally return the  list of results that were
produced. For  example, the expression "seqn [getChar, getChar, getChar]"  reads
three characters from the keyboard using the action getChar that  reads a single
character, and return a list containing the three characters.The interesting as-
pect of the function seqn is its type. One possible  type that can  be  inferred
from the above definition is the following: "seqn :: [IO a] → IO [a]".

This type states that seqn maps a list of IO (input/output) actions that produce
results of  some type a to a single  IO action that produces a  list of such re-
sults, which  captures the  high-level behaviour of seqn  in a clear and concise
manner. More importantly however, the type also makes explicit that the function
seqn involves the side effect of performing input/output actions. Using types in
this manner  to keep  a clear distinction  between functions  that are pure  and
those that involve side effects is a central aspect of Haskell,and brings impor-
tant benefits in terms of both programming and reasoning.

Chapter remarks
--------------------------------------------------------------------------------
[1] The Haskell report is freely available from the Haskell site.

[2] More detailed historical accounts of the development of functional languages
    in general, and Haskell in particular are given here:
    
https://pdfs.semanticscholar.org/e694/49921581f1e00b801994236f840f5b459e00.pdf
http://haskell.cs.yale.edu/wp-content/uploads/2011/02/history.pdf

Exercises
--------------------------------------------------------------------------------

