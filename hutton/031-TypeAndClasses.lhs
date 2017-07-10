Type and Classes
--------------------------------------------------------------------------------
Chapter 3 - Programming in Haskell by Graham Hutton

Basic concepts
--------------------------------------------------------------------------------
In this chapter we introduce types and classes, two of the most fundamental con-
cepts in Haskell. A type is a collection of related values.For example, the type
Bool contains the two logical values False, and True, while the type Bool → Bool
contains all functions that map arguments from Bool to results from Bool.

We use the notation "v :: T" to mean that "v" is a value in the type "T",and say
that "v" has type "T".
                                                    False :: Bool
                                                    True  :: Bool
                                                    not   :: Bool → Bool

More generally, the symbol "::" can also be used  with expressions that have not
yet been evaluated, in which case the notation "e :: T" means that evaluation of
the expression "e" will produce a value of type "T".
                                                    not True        :: Bool
                                                    not (not False)	:: Bool


In Haskell, every expression must have a type, which is calculated prior to eva-
luating the expression by a process called type inference. The key to  this pro-
cess is the following simple typing rule for function application,  which states
that if "f" is a function that maps arguments of type A to results of type B,and
"e" is an expression of type A, then the application "f e" has type B:

                            f :: A → B   e :: A  
                           ─────────────────────
                                  e :: B

For example, the typing of "not False :: Bool" can be inferred from this rule u-
sing the fact that "not :: Bool → Bool" and "False :: Bool". On the  other hand,
the expression "not 3" does not have a type under  the above rule,  because this
would require that "3 :: Bool",  which is not valid because "3" is not a logical
value. Expressions such as "not 3" that do not have a type are said to contain a
"type error", and are deemed to be invalid expressions.  Because type  inference
precedes evaluation, Haskell programs  are type safe, in the sense that type er-
rors can never occur during evaluation. The downside of type safety is that some
expression that evaluate successfully  will be rejected on type grounds. For ex-
ample, the conditional  expression "if True then 1 else False" evaluates to num-
ber "1", but contains a type error and is hence deemed invalid.  The typing rule
for a conditional expression  requires that both  possible results have the same
type.

In GHCi, the type of any expression can be displayed by preceding the expression
by the command ":type", for example:
                                                    Prelude> :type not
                                                    not :: Bool → Bool

                                                    Prelude> :type False
                                                    False :: Bool

                                                    Prelude> :type not False
                                                    not False :: Bool

Basic types
--------------------------------------------------------------------------------
Haskell provides a number  of basic types  that are built-in to the language, of
which the most commonly used are:

* Bool:    Logical values. This type  contains the two logical  values False and
           True.
* Char:    Single characters. This type  contains all  single characters  in the
           Unicode system.  As in most programming  languages, single characters
           must be enclosed in single forward quotes ''.
* String:  String of characters. This type contains all sequences of characters,
           such as "abc", "1 + 2 = 3", and  the empty  string "".  Again, as  is
           standard in most programming languages, strings of characters must be
           enclosed in double quotes "".
* Int:     Fixed-precision integers. This type contains integers such as -100,0,
           and 999, with fixed amount of memory being used for their storage.
* Integer: Arbitrary-precision integers. This type contains all integers,with as
           much memory as necessary  being used for their storage, thus avoiding
           the imposition of lower and upper limits on the range of numbers.
* Float:   Single-precision  floating-point  numbers. This type contains numbers
           with a decimal point, such as -12.34, 1.0, and 3.141592, with a fixed
           amount of  memory being  used for their storage.  The term "floating-
           point" comes from the  fact that the number of digits permitted after
           the decimal point depends upon the size of the number.
* Double:  Double-precision  floating-point  numbers. This  type  is similar  to
           Float, except that twice as much memory  is used for storage of these
           numbers to increase their precision.

Note that a single number may have more than one  numeric type. For example, the
number 3 could have type Int, Integer, Float, or Double. 

List types
--------------------------------------------------------------------------------
A list is a sequence of elements of the same type,with the elements being enclo-
sed in square parentheses and separated by commas. We write [T] for the  type of
all lists whose elements have type T. For example:
                                             [False, True, False]    :: [Bool]
                                             ['a', 'b', 'c', 'd']    :: [Char]
                                             ["One", "Two", "Three"] :: [String]

The number of elements in a list is called its length.The list [] of length zero
is called the empty list, while lists of length one, such as [False], ['a'], and
[[]] are called singleton lists.There are three points to note about list types:

1. The type of a list conveys no information about its length. [False, True],and
   [False, True, False] both have type [Bool], even  though they  have different
   lengths.
2. There are no restrictions on the type of the elements  of a list. We can have
   lists of lists, such as: 
                                   1.  [['a', 'b'], ['c', 'd', 'e']] :: [[Char]]
                                   2.  [[1, 3, 4], [5, 9], [10, 15]] :: [[Int]]
3. There is no restriction that a list must have a finite length. Due to the use
   of lazy evaluation in Haskell, lists with an infinite length are both natural
   and practical.

Tuple types
--------------------------------------------------------------------------------
A tuple is a finite sequence of components of possibly different types, with the
components being enclosed in round parentheses and separated by commas. We write
(T1, T2, .. ,Tn) for the type of all tuples whose ith components have type  "Ti"
for any i in the range 1 to n. For example:
                                    (False, True)       :: (Bool, Bool)
                                    (False, 'a', True)  :: (Bool, Char, Bool)
                                    ("Yes", True, 'a')  :: (String, Char, Bool)

The number of components in a tuple  is called its  arity. The tuple () of arity
zero is called the empty tuple, tuples of  arity two are called pairs, tuples of
arity three are called triples, and so on. Tuples of arity one, such as (False),
are not permitted  because they would conflict  with the use  of parentheses  to
make the evaluation order explicit. There are three points to note:

1. The type of a tuple conveys its arity. For example, the type (Bool,Char) con-
   tains all pairs comprising a first component of type Bool and a second compo-
   nent of type Char.
2. There are no restrictions on the types of the components of a tuple.  We  can
   now have tuples of tuples, tuples of lists, and lists of tuples:
                       1. ('a', (False, 'b'))            :: (Char, (Bool, Char))
                       2. (['a', 'b'], [False, True])    :: ([Char], [Bool])
                       3. [('a', False), ('b', True)]    :: [(Char, Bool)]
3. Note that  tuples must  have a finite  arity, in order to  ensure that tuples
   types can always be inferred prior to evaluation.

Function types
--------------------------------------------------------------------------------
A function is a mapping from arguments of one type to results of another type.We
write "T1 → T2" for the type of all functions that map arguments of type "T1" to
results of type "T2". For example:
                                                             not  :: Bool → Bool
                                                             even :: Int →  Bool

Because there are no restrictions on the types of the arguments and results of a
function, the simple notion of a function  with a single  argument and  a single
result is already sufficient to handle  the case of  multiple arguments  and re-
sults, by packaging multiple values using lists or tuples.  For example, we  can
define a function add that calculates the sum of a pair of integers, and a func-
tion zeroto that returns the list of integers from zero to a given limit:

> add :: (Int, Int) -> Int
> add (x, y) = x + y
>
> zeroto :: Int -> [Int]
> zeroto n = [0..n]

In these examples we have followed  the Haskell convention of preceding function
definitions by their types, which serves as useful documentation. Any such types
provided manually by the user are checked for consistency  with the types calcu-
lated automatically using type inference. Note that there is no restriction that
functions must be total on their  argument type, in the sense  that there may be
some arguments for which the result is not defined.For example the result of the
library function head that selects the first  element of a list is  undefined if
the list is empty:
                                         Prelude> head []
                                         *** Exception: Prelude.head: empty list


Curried Functions
--------------------------------------------------------------------------------
Functions  with multiple  arguments can also be handled in another, perhaps less 
obvious way, by exploiting the fact that functions  are free to return functions
as results. For example, consider the following definition:

> add' :: Int -> (Int -> Int)
> add' x y = x + y

The type states that  add' is a function that takes an argument of type Int, and
returns a result that is a function of type "Int → Int". The  definition  itself
states  that add' takes an integer x followed  by an  integer y, and returns the
result x + y. More precisely, add' takes  an integer x and  returns  a function,
which in turn takes an integer y and returns the result x+y. Note that the func-
tion add' produces the same final result as the function add  from the  previous
section, but whereas add takes its two arguments at the same time  packaged as a
pair, add' takes its two arguments one at  a time, as reflected in the different
types of the two functions.
                                            add  :: (Int, Int) → Int
                                            add' :: Int → (Int → Int)

Functions with  more than two arguments can also be handled using the same tech-
nique, by returning functions that return functions, and  so on. For  example, a
function mult that takes three integers x, y, and z, one at  a time, and returns
their product, can be defined as follows:

> mult :: Int -> (Int -> (Int -> Int))
> mult x y z = x * y * z

This definition states that mult takes an integer x and returns a function,which
in turn takes an integer y and returns another function, which  finally takes an
integer z and returns  the result "x * y * z". Functions such  as add' and  mult
that take their arguments one at a time are called curried functions. As well as
being interesting in their own  right, curried  functions are also more flexible
than functions on tuples,because useful functions can often be made by partially
applying a curried function with less than its full complement arguments.  
For example, a function that increments an integer  can be given  by the partial
application "add' 1 :: Int → Int"  of the curried function add' with only one of
its arguments. To avoid excess parentheses  when working with curried functions,
two simple conventions are adopted:

1. The  function  arrow → in types is assumed to associate to the right. For ex-
   ample, the type:
                                            Int → Int → Int → Int
   means:
                                            Int → (Int → (Int → Int))
   
2. Consequently, function  application, which is denoted silently using spacing,
   is assumed to associate to the left. For example, the application:

                                            mult x y z
   means:
                                            ((mult x) y) z
   
Unless tupling is explicitly  required, all functions in  Haskell with  multiple
arguments are  normally  defined as curried functions, and the  two  conventions
above are used to reduce the number of parentheses that are required.

Polymorphic Types
--------------------------------------------------------------------------------
The library function length calculates  the length of  any list, irrespective of
the type of the elements of the  list. For example, it can be  used to calculate
the length of a list of integers, a list of strings,or even a list of functions:

                                            Prelude> length [1,3,5,7]
                                            4
                                            Prelude> length ["Yes","No"]
                                            2
                                            Prelude> length [sin,cos,tan]
                                            3

The idea  that length can be  applied to lists whose elements have  any type  is
made precise in its type by the inclusion of a type variable.Type variables must
begin with a lower-case letter, and are usually simply named a, b, c, and so on.
For example, the type of length is:
                                            Prelude> :type length
                                            length :: [a] → Int

That is, for any type a, the function length has type "[a] → Int".  A type  that
contains  one or  more type  variables is  called polymorphic (meaning  "of many
forms"). Hence, "[a] → Int" is a polymorphic type  and length  is a  polymorphic
function. Other polymorphic functions are:
                                            fst   :: (a, b) → a
                                            head  :: [a] → a
                                            take  :: Int → [a] → [a]
                                            zip   :: [a] → [b] → [(a, b)]
                                            id    :: a → a

The type of a  polymorphic function  often  gives a strong indication  about the
function's behaviour. For example, from the type "[a] → [b] → [(a, b)]"  we  can
conclude that zip pairs up elements from two lists, although the type on its own
doesn't capture the precise manner in which this is done.

Overloaded Types
--------------------------------------------------------------------------------
The arithmetic operator + calculates the sum of any two numbersw of the same nu-
meric type. For example, it can be used to calculate the sum of two integers, or
the sum of two floating-point numbers:
                                            Prelude> 1 + 2
                                            3
                                            Prelude> 1.0 + 2.0
                                            3.0

The idea that + can be applied to numbers of any numeric type is made precise in
its type by the inclusion of a "class constraint". Class  contraints are written
in the form "C a", where "C" is the name of a class and "a" is a  type variable.
For example, the type of the addition operator + is as follows:
                                                        Prelude> :type (+)
                                                        (+) :: Num a ⇒ a → a → a

That is, for any type a that  is an instance of the  class Num of numeric types,
the function (+) -parethesising an operator converts it into a curried function-
has type "a → a → a". A type that contains one or more class constraints is cal-
led overloaded, as is an expression with such a type.

Hence, "Num a ⇒ a → a → a" is an overloaded  type and (+) is an overloaded func-
tion. Most of the numeric functions  provided in the prelude are overloaded. For
example:                                    Prelude> :type (*)
                                            (*) :: Num a ⇒ a → a → a
                                            Prelude> :type negate
                                            negate :: Num a ⇒ a → a
                                            Prelude> :type abs
                                            abs :: Num a ⇒ a → a

Numbers themselves are also overloaded. For example:
                                            Prelude> :type 3
                                            3 :: Num a ⇒ a

Means that for any numeric type a, the  value 3 has type a.In this maner,the va-
lue 3 could be an integer, a floating-point number, or more generally a value of
any numeric type, depending on the contet in which it is used.

Basic Classes
--------------------------------------------------------------------------------
Recall that a type is a collection of related values. Building upon this notion,
a class is a collection of types that support certain overloaded operations cal-
led methods. Haskell provides a number of basic classes that are built-in to the
language, of which the most commonly used are described below.

* Eq - equality types: This class contains type whose values can be compared for
  equality and inequality using the following two methods:
  (==) :: a → a → Bool
  (/=) :: a → a → Bool
  
  All  the  basic types Bool, Char, String, Int, Integer, Float, and  Double are
  instances of the Eq class, as  are lists and tuples types, provided that their
  elements and component types are instances. For example:

  Prelude> False == False
  True
  Prelude> 'a' == 'b'
  False
  Prelude> "abc" == "abc"
  True
  Prelude> [1,2] == [1,2,3]
  False
  Prelude> ('a',False) == ('a',False)
  True
  
  Note that function types are not in general instances of the Eq class, because
  it is not feasible in general to compare two functions for equality.

* Ord - ordered types: This class contains types that are instances of the equa-
  lity class Eq, but in  addition whose values  are totally  (linearly) ordered,
  and as such can be compared and processed using the following six methods:
  (<)  :: a → a → Bool
  (<=) :: a → a → Bool
  (>)  :: a → a → Bool
  (>=) :: a → a → Bool
  min  :: a → a → a
  max  :: a → a → a
  
  All  the basic  types  Bool, Char, String, Int, Integer, Float, and Double are
  instances of the Ord class, as are lists types and  tuple types, provided that
  their elements and component types are instances. For example:

  Prelude> False < True
  True
  Prelude> min 'a' 'b'
  'a'
  Prelude> "elegant" < "elephant"
  True
  Prelude> [1,2,3] < [1,2]
  False
  Prelude> ('a',2) < ('b',1)
  True
  Prelude> ('a',2) < ('a',1)
  False
  
  Note that strings, lists and tuples are ordered lexicographically, that is, in
  the same way as words in a dictionary. For example, two pairs of the same type
  are in order if their first components are in  order, in which  case their se-
  cond components are not considered, or if their first components are equal, in
  which case their second components must be in order.

* Show - showable types: This class contains types whose values can be converted
  into strings of characters using the following method:
  show :: a → String
  
  All  the basic  types Bool, Char, String, Int, Integer, Float,  and Double are
  instances of the Show class, as are list types and  tuple types, provided that
  their elements and component types are instances. For example:

  Prelude> show False
  "False"
  Prelude> show 'a'
  "'a'"
  Prelude> show [1,2,3]
  "[1,2,3]"
  Prelude> show ('a',False)
  "('a',False)"
  
* Read - readable types: This class is dual to Show,and contains types whose va-
  lues can be converted from strings of characters using the following method:
  read :: String → a
  
  All  the basic  types Bool, Char, String, Int, Integer, Float,  and Double are
  instances of the Show class, as are list types and  tuple types, provided that
  their elements and component types are instances. For example:

  Prelude> read "False" :: Bool
  False
  Prelude> read "'a'" :: Char
  'a'
  Prelude> read "123" :: Int
  123
  Prelude> read "[1, 2, 3]" :: [Int]
  [1, 2, 3]
  Prelude> read "('a', False)"  :: (Char, Bool)
  ('a', False)
  
  The use of :: in these  examples resolves the type  of the result, which would
  otherwise not be able to be inferred by GHCi. In practice, however, the neces-
  sary type information can usually be inferred automatically from  the context.
  For  example, the  expression "not (read "False")" requires  no explicit  type
  information, because the  application of the logical negation function not im-
  plies that "read "False"" must have type Bool. Note that the result of read is
  undefined if its arguments is not syntactically valid.For example, the expres-
  sion "not (read "abc")" produces an error when evaluated, because "abc" cannot
  be read as a logical value:
                                    Prelude> not (read "abc")
                                    *** Exception: Prelude.read: no parse

* Num - numeric types: This class contains type whose values are numeric, and as
  such can be processed using the following six methods:
  (+)      :: a → a → a
  (-)      :: a → a → a
  (*)      :: a → a → a
  negate   :: a → a
  abs      :: a → a
  signum   :: a → a
  
  The basic types Int, Integer, Float,and Double are instances of the Num class.
  For example:

  Prelude> 1 + 2
  3
  Prelude> 1.0 + 2.0
  3.0
  Prelude> negate 3.0
  -3.0
  Prelude> abs (-3)
  3
  Prelude> signum (-3)
  -1

  Negative numbers must be parenthesised when used as arguments to functions, to
  ensure the correct interpretation of the minus sign.For example,"abs -3" with-
  out parentheses means "abs - 3", which is both the incorrect meaning here  and
  an ill-typed expression. Note that the Num class does not  provide a  division
  method. Division  is handled separately using two special classes, one for in-
  tegral numbers and one for fractional numbers.

* Integral - integral types: This class contains types that are instances of the
  numeric class Num, but in addition whose values are integers, and as such sup-
  port the methods of integer division and integer remainder:
  div :: a → a → a
  mod :: a → a → a
  
  In practice,these two methods are often written between their two arguments by
  enclosing their  names in single back quotes. The basic types Int, and Integer
  are instances of the Integral class. For example:

  Prelude> 7 `div` 2
  3
  Prelude> 7 `mod` 2
  1
  
  For efficiency  reasons, a number of prelude functions that involve both lists
  and integers (such as take, and drop) are restricted to the type Int of finite
  precision  integers, rather than being applicable to any instance of the Inte-
  gral class. If required, however, such generic versions of these functions are
  provided as part of an additional library file called Data.List.

* Fractional - fractional types: This class contains types that are instances of
  the numeric class Num, but in addition whose  values are non-integral,  and as
  such support the methods of fractional division and fractional reciprocation:
  (/)   :: a → a → a
  recip :: a → a
  
  The basic types Float and Double are instances. For example:

  Prelude> 7.0 / 2.0
  3.5
  Prelude> recip 2.0
  0.5
  
Chapter Remarks
--------------------------------------------------------------------------------
[1] The term Bool for the type of logical  values celebrates the pioneering work
of George Boole on symbolic logic,while the term curried for functions that take
their arguments one at a time celebrates the work of Haskell Curry.

[2] The relationship between the type of a polymorphic function and its behavior
is formalised in the paper:
  P. Wadler, "Theorems for Free!" in Proceedings of the International Conference
  on Functional Programming and Computer Architecture. ACM Press, 1989.

[3] A more detailed account of the type system is given in the Haskell Report:
  S. Marlow, Ed., "Haskell Language Report, 2010"

[4] A formal description of the type system can be found in:
  M. P. Jones, "Typing Haskell in Haskell"
  in Proceedings of the Haskell Workshop. University of Utrecht,
  Technical Report UU-CS-1999-28, 1999.

[5] For more information on the type inference algorithm used by Haskell see: 
  Hindley-Miller type system

Exercises
--------------------------------------------------------------------------------
1. What are the types of the following values:
   Prelude> :type ['a','b','c']
   ['a','b','c'] :: [Char]

   Prelude> :type ('a','b','c')
   ('a','b','c') :: (Char,Char,Char)

   Prelude> :type [(False,'0'), (True,'1')]
   [(False,'0'), (True,'1')] :: [(Bool, Char)]

   Prelude> :type ([False,True], ['0','1'])
   ([False,True], ['0','1']) :: ([Bool],[Char])

   Prelude> :type [tail,init,reverse]
   [tail,init,reverse] :: [[a] → [a]]
   

2. Write down definitions that have the following types;it does not matter what
   the definitions actually do as long as they are type correct.

> bools :: [Bool]
> bools = [False, True, True]
>
> nums :: [[Int]]
> nums = [ [1, 2, 3]
>        , [4, 5]
>        , [9, 10]
>        ]
>          
> addi :: Int -> Int -> Int -> Int
> addi x y z = x + y + z
>
> copy' :: a -> (a, a)
> copy' x = (x, x)
>
> apply :: (a -> b) -> a -> b
> apply f x = f x

3. What are the types of the following functions?

> second :: [a] -> a
> second xs = head (tail xs)
>
> swap :: (a, b) -> (b, a)
> swap (x, y) = (y, x)
>
> pair :: a -> b -> (a, b)
> pair x y = (x, y)
>
> double :: Num a => a -> a
> double x = x * 2
>
> palindrome :: String -> Bool
> palindrome xs = reverse xs == xs
>
> twice :: (a -> a) -> a -> a
> twice f x = f (f x)

4. Check your answers to the preceding three questions using GHCi.

5. Why is it not feasible in general for function types to be instance of the Eq
   class? When it is feasable? (Hint:two functions of the same type are equal if
   they always return equal results for equal arguments).

   Because that will mean that both are identical functions, performing the same
   operation. It is feasable in this case:

> prod  :: Num a => a -> a -> a
> prod  x y = x * y
>
> prod' :: Num a => a -> a -> a
> prod' y x = y * x
>
> tester :: Bool
> tester = prod 5 4 == prod' 5 4

