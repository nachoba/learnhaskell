Defining Functions
--------------------------------------------------------------------------------
Chapter 4 - Programming Haskell by Graham Hutton

New from old
--------------------------------------------------------------------------------
Perhaps the  most straightforward way to define new functions is simply by combi-
ning one ore more existing functions. For example, a few  library functions that
can be defined in this way:

> -- Decide if an integer is even
> even' :: Integral a => a -> Bool
> even' n = n `mod` 2 == 0
>
> -- Split a list at the nth element
> splitAt' :: Int -> [a] -> ([a], [a])
> splitAt' n xs = (take n xs, drop n xs)
>
> -- Reciprocation
> recip' :: Fractional a => a -> a
> recip' n = 1 / n

Note that the above functions already exist in the Prelude,therefore we have de-
fined our new ones using a single quote.Note the use of class constraints in the
types for even' and recip', which make precise the idea that these functions can
be applied to numbers of any Integral and Fractional types, respectively.

Conditional expressions
--------------------------------------------------------------------------------
Haskell provides  a range of  different ways to define functions that choose be-
tween a number  of possible results.  The simplest  are conditional expressions,
which use a logical expression called a condition to choose between two  results
of the same type. If the condition is True, then the first result is chosen, and
if it is False, then the second result is chosen. For example, the library func-
tion "abs" that returns the absolute value of  an integer can be defined as fol-
lows:

> abs' :: Int -> Int
> abs' n = if n >= 0 then n else -n

Conditional expression may  be nested, in the sense that they  can contain other
conditional expressions. For example, the library function "signum" that returns
the sign of an integer can be defined as follows:

> signum' :: Int -> Int
> signum' n = if n < 0 then -1 else
>                if n == 0 then 0 else 1

Note that unlike in some programming languages,conditional expression in Haskell
must always have an else branch,which avoids the well-know dangling else problem.

Guarded equations
--------------------------------------------------------------------------------
As an alternative to using conditional expressions,functions can also be defined
using "guarded equations", in which a sequence of logical expressions called gu-
ards is used to choose between a sequence of results of the same type. 
If the first guard is True, then the first result is chosen;otherwise,if the se-
cond is True, then the second result is chosen, and so on. For example,  the li-
brary function abs can also be defined using guarded equations as follows:

> abs'' :: Int -> Int
> abs'' n | n >= 0        =  n
>         | otherwise     = -n

The symbol | is read as "such that", and the guard "otherwise" is defined in the
standard prelude simply by "otherwise = True". Ending a sequence of  guards with 
otherwise is not necessary, but provides a convenient way  of handling all other
cases, as well  as avoiding the possibility that  none of the guards in  the se-
quence is True, which would otherwise result in an error. The  main  benefit  of
guarded equations over conditional expressions is that definitions with multiple
guards are easier to read. For example, the library function signum is easier to
understand when defined as follows:

> signum'' :: Int -> Int
> signum'' n | n <  0     = -1
>            | n == 0     =  0
>            | otherwise  =  1

Pattern matching
--------------------------------------------------------------------------------
Many functions have a simple and intuitive definition using pattern matching, in
which a sequence of syntactic  expressions called patterns is used to choose be-
tween a  sequence of  results of the same type. If the first pattern is matched,
then the first result is chosen; otherwise, if the  second is  matched, then the
second result is chosen, and so on. For example, the library function "not" that
returns the negation of a logical value can be defined as follows:

> not' :: Bool -> Bool
> not' False = True
> not' True  = False

Functions with more than one argument can also be defined using pattern matching
,in which case the patterns for each  argument are  matched in order within each
equation. For example, the library operator "&&" that returns conjunction of two
logical values can be defined as follows (we will use &&& to distinguish it from
the library function &&)

> (&&&) :: Bool -> Bool -> Bool
> True  &&& True   = True
> True  &&& False  = False
> False &&& False  = False
> False &&& True   = False

However, this definition can be simplified by combining the last three equations
into a single equation that returns False independent of the values  of the  two
arguments, using the wildcard pattern "_" that matches any value:

> (&&&&) :: Bool -> Bool -> Bool
> True  &&&& True  = True
> _     &&&& _     = False

This version also has the benefit that, under lazy evaluation,if the first argu-
ment is False,then the result False is returned without the need to evaluate the
second argument. In practice, the prelude defines "&&" using equations that have
this same property, but make the choice about  which equations applies using the
value of the first argument only:

> (&&&&&) :: Bool -> Bool -> Bool
> True  &&&&& b = b
> False &&&&& _ = False

That is, if  the first argument is True, then the result is the value of the se-
cond argument, and, if the first argument if False,then the result is False. Not
that Haskell does not permit the same name to be used for more than one argument
in a single equation. For example, the following definition for  the operator &&
is based upon the observation that, if the two logical arguments are equal, then
the result  is the same value, otherwise the result is False, but is invalid be-
cause of the above naming requirements:
                                                    (&-&) :: Bool → Bool → Bool
                                                    b &-& b = b
                                                    _ &-& _ = False

Gives the error:                • Conflicting definitions for ‘b’
                                  Bound at: 041-DefiningFunctions.lhs:113:1
                                  041-DefiningFunctions.lhs:113:7
                                • In an equation for ‘&-&

If desired,however, a valied version of this definition can be obtained by using
a guard to decide if the two arguments are equal:

> (&-&) :: Bool -> Bool -> Bool
> b &-& c | b == c    = b
>         | otherwise = False

So far, we have only considered basic patterns that are either values, variables
,or the wildcard pattern. In the remainder of this section we introduce two use-
ful ways to build larger patterns by combining smaller patterns.

Tuple patterns
--------------------------------------------------------------------------------
A tuple of patterns is itself a pattern,which matches any tuple of the same ari-
ty whose components all match the corresponding patterns in order.  For example,
the library functions "fst" and "snd" that respectively select the first and se-
cond components of a pair are defined as follows:

> fst' :: (a, b) -> a
> fst' (x,_) = x
>
> snd' :: (a, b) -> b
> snd' (_,y) = y

List patterns
--------------------------------------------------------------------------------
Similarly, a list of patterns is itself a pattern, which matches any list of the
same length whose elements all match the corresponding patterns in order.For ex-
ample, a function test that decides if a list contains precisely three character
beginning with the letter 'a' can be defined as follows:

> test :: [Char] -> Bool
> test ['a',_,_] = True
> test _         = False

Up to this point, we have viewed lists as a primitive notion in Haskell. In fact
they are not primitive as such, but are constructed one  element at a time star-
ting from the empty list [] using an operator : called "cons" that constructs  a
new list by prepending a new element to the start of an  existing  list. For ex-
ample, the list [1,2,3] can be decomposed as follows:

  [1, 2, 3]
=      { list notation }
  1 : [2, 3]
=      { list notation }
  1 : (2 : [3])
=      { list notation }
  1 : (2 : (3 : []))

That is, [1,2,3] is just an abbreviation for 1:(2:(3:[])).To avoid excess paren-
theses when working with such lists, the cons operator is  assumed to  associate
to the right.For example if you take the expression 1:2:3:[] means 1:(2:(3:[])).

Prelude> :info (:)
data [] a = ... | a : [a] 	-- Defined in ‘GHC.Types’
infixr 5 :

As well as being  used to construct lists, the cons operator can also be used to
construct  patterns, which match any non-empty list whose first and remaining e-
lements match  the corresponding patterns  in order. For example, we can now de-
fine a more general version  of the function test that decides if a list contai-
ning any number of characters begins with the letter 'a':

> test' :: [Char] -> Bool
> test' ('a':_) = True
> test' _       = False

Similarly, the library  functions head and tail that respectively select and re-
move the first element of a non-empty list are defined as follows:

> head' :: [a] -> a
> head' (x:_) = x
>
> tail' :: [a] -> [a]
> tail' (_:xs) = xs

Note that cons patterns  must be parenthesised, because function application has
higher priority than all other operators in the language.For example the defini-
tion "head x:_ = x" without parentheses means, in reality "(head x):_ = x" which
is both the incorrect meaning and an invalid definition.

Lambda expressions
--------------------------------------------------------------------------------
As an  alternative to  defining functions using equations, functions can also be
constructed using lambda expressions, which comprise a  pattern  for each of the
arguments,a body that specifies how the result can be calculated in terms of the
arguments, but do not give a name for the function itself.In other words, lambda
expressions are nameless functions, or anonymous functions.

For example, the nameless function that takes a single number x as its argument,
and produces the result x + x, can be constructed as follows:

\x -> x + x

The symbol \ represents  the Greek letter lambda, written as λ. Despite the fact
that they have no names, functions constructed  using lambda  expression  can be
used in the same way as any other functions. For example:

Prelude> (\x -> x + x) 2
4

As well as being interesting in their own right,lambda expressions have a number
of practical  applications. First of all, they can be used to formalise the mea-
ning of curried function definitions. For example, the definition:

addd :: Int → Int → Int
addd x y = x + y

Can be undesrtood as meaning:

> addd :: Int -> (Int -> Int)
> addd = \x -> (\y -> x + y)

Which makes precise that addd is a function that takes an integer x and  returns
a function, which in turn takes another integer y and returns the result  x + y.
Moreover, rewriting the original definition  in this manner also has the benefit
that  the type for  the function and the manner in which it is defined now  have
the same syntactic form, namely ? → (? → ?).

Also, lambda expressions  are also  useful when  defining  functions that return
functions  as results by their very nature, rather than as a consequence of cur-
rying. For example, the library function const that returns a constant  function
that always produces a given value can be defined as follows:

> const' :: a -> b -> a
> const' x _ = x

However, it is more appealing to define const in a  way that makes explicit that
it returns a function as its result, by including parentheses in the type and u-
sing a lambda expression in the definition itself:

> const'' :: a -> (b -> a)
> const'' x = \_ -> x

Finally, lambda expressions can be used to  avoid having to name a function that
is only referenced once in a program. For example, a function odds that  returns
the first n odds integers can be defined as follows:

> odds :: Int -> [Int]
> odds n = map f [0..(n-1)]
>           where 
>             f x = x * 2 + 1

Note that the library function map applies a function to all elements of a list.
However, because the locally defined function f is only referenced once, the de-
finition for odds can be simplified by using a lambda expression:

> odds' :: Int -> [Int]
> odds' n = map (\x -> x * 2 + 1) [0..(n-1)]

Operator sections
--------------------------------------------------------------------------------
Functions such as + that are written between their two arguments are called ope-
rators. As we have already seen,any function with two arguments can be converted
into an operator by enclosing the name of the function in single back quotes, as
in:             7 `div` 2

However, the converse is also possible.In particular,any operator can be conver-
ted into a curried function that's written before its arguments by enclosing the
name of the operator in parentheses, as in "(+) 1 2".  Moreover, this convention
also allows one of the arguments to be included in the parentheses if desired,as
in "(1+) 2" and "(+2) 1".

In general if # is an operator, then expressions of the form (#), (x#), and (#y)
for arguments x and y are called "sections", whose  meaning as  functions can be
formalised using lambda expressions as follows:

(#)  = \x → (\y → x # y)
(x#) = \y → x # y
(#y) = \x → x # y

Sections have three primary applications.First of all, they can be used to cons-
truct a number of simple but useful functions in  a particularly compact way, as
shown in the following examples:

(+)  is the addition function      \x → (\y → x + y)
(1+) is the successor function     \y → 1 + y
(1/) is the reciprocation function \y → 1 / y
(*2) is the doubling function      \x → x * 2
(/2) is the halving function       \x → x / 2

Secondly, sections  are necessary when stating the type of operators, because an
operator itself is not a valid expression in Haskell.For example,the type of the
addition operator + for integer is stated as follows:

(+) :: Int → Int → Int

Finally, sections are also necessary when using operators as  arguments to other
functions. For example, the library function sum  that calculates  the sum  of a
list  of integers can  be defined  by using the operator + as an argument to the
library function foldl.

> suma :: [Int] -> Int
> suma = foldl (+) 0

Chapter remarks
--------------------------------------------------------------------------------
[1] A formal  meaning for  pattern matching  by translation using more primitive
    features of the language is given in the Haskell Report

[2] The Greek letter λ( lambda) used when defining nameless functions comes from
    the lambda calculus, the mathematical theory of functions upon  which Haskell
    is founded. In particular see "The Lambda Calculus, its Syntax and Semantics"
    H.P. Barendregt.

Exercises
--------------------------------------------------------------------------------
1. Using library functions, define a function  "halve :: [a] → ([a], [a])"  that
   splits an even-lengthed list into two halves. For example:

   Prelude> halve [1,2,3,4,5,6]
   ([1,2,3], [4,5,6])
   
> -- Answer
> halve :: [a] -> ([a], [a])
> halve xs = if (length xs `mod` 2 == 0) 
>              then (split' xs) 
>            else error "not even list"
>            where
>            split' xs = (take 3 xs, drop 3 xs)

2. Define a function "third :: [a] → a" that returns the third element in a list
   that contains at least this many elements using:

   1. head and tail
   2. list indexing !!
   3. pattern matching

> -- Using head and tail
> third :: [a] -> a
> third xs = if (length xs >= 3) then (tercero xs) else error "List too short"
>              where
>              tercero xs = head (tail (tail xs))
>
> -- Using list indexing !!
> third' :: [a] -> a
> third' xs = if (length xs >= 3) then (tercero xs) else error "List too short"
>               where
>               tercero xs = xs !! 2
>                   
> -- Using pattern matching
> third'' :: [a] -> a
> third'' []        = error "Empty list"
> third'' [_]       = error "List too short"
> third'' [_,_]     = error "List too short"
> third'' (_:_:a:_) = a

3. Consider a function  "safetail :: [a] → [a]"  that behaves in the same way as
   tail except that it maps the empty list to itself rather than producing an e-
   rror. Using tail and the function "null :: [a] → Bool" that decides if a list
   is empty or not, define safetail using:

   1. a conditional expression
   2. guarded equations
   3. pattern matching

> -- Using conditional expressions
> safetail :: [a] -> [a]
> safetail xs = if (null xs) then [] else (tail xs)  
>
> -- Using guarded equations
> safetail' :: [a] -> [a]
> safetail' xs | null xs   = []
>              | otherwise = tail xs
>             
> -- Using pattern matching
> safetail'' :: [a] -> [a]
> safetail'' [] = []
> safetail'' (_:xs) = xs

4. In a similar way to what we did with the && operator,show how the disjunction
   operator || can be defined in four different ways using pattern matching.

> -- First form
> (|||) :: Bool -> Bool -> Bool
> True  ||| True  = True
> True  ||| False = True
> False ||| True  = True
> False ||| False = False
>
> -- Second form
> (||||) :: Bool -> Bool -> Bool
> False |||| False = False
> _     |||| _     = True
>
> -- Third form
> (|||||) :: Bool -> Bool -> Bool
> False ||||| b = b
> _     ||||| _ = True
>
> -- Fourth form
> (|-|) :: Bool -> Bool -> Bool
> b |-| c | b == c    = b
>         | otherwise = True

5. Without using any other library functions or operators, show how the  meaning
   of the following pattern matching definition for  logical  conjunction && can
   be formalised using conditional expressions:

   True  && True = True
   _     && _    = False
   
   Hint: use two nested conditional expression.

> and' :: Bool -> Bool -> Bool
> and' x y = if (x==True) then (if (y==True) then True else False) else False

6. Do the same for the following  alternative definition, and note the difference
   in the number of conditional expressions that are required:

   True  && b = b
   False && _ = False
   
> and'' :: Bool -> Bool -> Bool
> and'' x y = if (x == True) then y else False

7. Show how the meaning of the following curried function definition can be for-
   malised in terms of lambda expressions:

   mult :: Int → Int → Int → Int
   mult x y z = x * y * z
   
> multi :: Int -> (Int -> (Int -> Int))
> multi = (\x -> (\y -> (\z -> x * y * z)))
   
8. The Luhn algorithm  is used to check bank card numbers for simple errors such
   as mistyping a digit, and proceeds as follows:

   1. consider each digit as a separate number
   2. double every second number starting from the second last and working back-
      wards
   3. subtract 9 from each number that is now greater than 9
   4. add all the resulting numbers together
   5. if the total is divisible by 10, the card number is valid

   Define a function  "luhnDouble :: Int → Int"  that  doubles a digit  and sub-
   tracts 9 if the result is greater than 9. For example:

   Prelude> luhnDouble 3
   6
   Prelude> luhnDouble 6
   3
   
> luhnDouble :: Int -> Int
> luhnDouble x = if (x * 2 > 9) then (x * 2 - 9) else (x * 2)

   Using lugnDouble and the integer remainder function `mod`, define a function:
   "luhn :: Int → Int → Int → Int → Bool" that decides if a four-digit bank card
   number is valid. For example:

   Prelude> luhn 1 7 8 4
   True
   Prelude> luhn 4 7 8 3
   False
   
> luhn :: Int -> Int -> Int -> Int -> Bool
> luhn w x y z = if (total `mod` 10 == 0) then True else False
>                  where
>                     total = luhnDouble w + x + luhnDouble y + z


