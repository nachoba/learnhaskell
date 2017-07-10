Higher Order Functions
-------------------------------------------------------------------------------

> module HigherOrder where

Take a look at this simple function:


> multThree :: (Num a) => a -> a -> a -> a
> multThree x y z = x * y * z

If you see the signature  of this function it takes  three numbers, and  returns
another number.  However, we can partially  apply  this function.  For instance:

> multTwoWithNine = multThree 9

Prelude> :type multTwoWithNine
multTwoWithNine :: Num a ⇒ a → a → a

We see that now this function has one less argument. Let's continue in this line

> multWithEighteen = multTwoWithNine 2

Prelude> :type multWithEighteen
multWithEighteen :: Num a → a → a
Prelude> multWithEighteen 10
180

It's just a function that takes one argument and returns a number as the result.
This is call 'currying'.

Suppose we want to create a function that takes a number and compares it to 100.
We could do something like this:

> compareWithHundred :: (Num a, Ord a) => a -> Ordering
> compareWithHundred x = compare 100 x

Notice that the  x is on the right  hand side on both sides of the equation. Now   
let's look at what is the type of the following expression:

> compHun = compare 100

Prelude> :type compHun
compHun :: Integer → Ordering

Some more curried functions:

> divideByTen :: (Floating a) => a -> a
> divideByTen = (/10)

Infix functions can also be  partially applied by using sections.  To section an
infix function, simply surround it with parentheses  and only supply a parameter 
on one side. The only special thing about sections is using the minus sign -.

> isUpperAlphanum :: Char -> Bool
> isUpperAlphanum = (`elem` ['A'..'Z'])


- Some higher-orderism is in order
Functions  can take functions as parameters and also return functions. To illus-
trate this,  we're going to  make a function that  takes a function and then ap-
plies it twice to something:

> applyTwice :: (a -> a) -> a -> a
> applyTwice f x = f ( f x )

First of all, notice the type declaration.  In the previous examples  we did not
need parentheses because → is  naturally right-associative.  However, here, they
are mandatory. They indicate that the first  parameter  is a function that takes
something and returns that same thing. The second parameter is something of that
type also and the return value is also of the same type.
We will say that this function  takes two parameters and  returns one thing. The
first parameter  is a  function (of  type a → a ) and the second is that same a.
The function  can also  be Int → Int, or String → String or whatever.  But then,
the second parameter also has to be of that type. Let's  play a  little with the
function:

Prelude> applyTwice (+3) 10
16
Prelude> applyTwice (++ "HaHa") "Hey"
"HeyHaHaHaHa"
Prelude> applyTwice (multThree 2 2) 9
144
Prelude> applyTwice (3:) [1]
[3,3,1]

The awesomeness and usefulness of  partial application is evident.  If our func-
tion requires  us to  pass it a function  that takes only one parameter,  we can
just  partially apply a  function to the point  where it takes  only one parame-
ter and then pass it. Now we are going to use higher order programming to imple-
ment a really useful function that is in the standard library.
It is called zipWith and  takes a function and two lists  as parameters and then
joins the two lists by applying the function between corresponding elements.

> zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
> zipWith' _ [] _          = []
> zipWith' _ _  []         = []
> zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

Look at the type declaration.  The first parameter  is a function that takes two
things and produces a third thing. They do not have to be of the  same type, but
they can. The second and third  parameter are  lists. The result is also a list.
The first has to be a list of a's because the joining  function takes a's as its
first argument. The second has to be a list of b's, because the second parameter
of the joining function is of type b.  The result is a list of c's.  If the type
delcaration of a function says it accepts and a → b → c function  as a parameter
, it will also accept and a → a → a function, but not the other way around!.
Remember that when you are making  functions, especially higher order  ones, and
you are unsure of  the type, you can just try omitting  the type declaration and
then checking what Haskell infers it to be using :type.
The action in the function is pretty similar to the normal zip. The  edge condi-
tions are the  same, only there is an extra argument, the  joining function, but
that argument does not matter in the edge conditions, so we just use a _ for it.
And function body at the last pattern is also similar to zip, only it doesn't do
(x,y), but f x y. A single higher order function can be  used for a multitude of
different tasks if it is general  enough. Here is a little  demonstration of all
the different things our zipWith' function can do:

Prelude> zipWith' (+) [4,2,5,6] [2,6,2,3]
[6,8,7,9]
Prelude> zipWith' max [6,3,2,1] [7,3,1,5]
[7,3,2,5]
Prelude> zipWith' (++) ["foo ","bar ","baz "] ["fighters","hoppers","aldrin"]
["foo fighters","bar hoppers","baz aldrin"]
Prelude> zipWith' (*) (replicate 5 2) [1..]
[2,4,6,8,10]

As you see, a single higher order function can  be used in  very versatile ways.
Imperative programming usually uses stuff  like for loops, while  loops, setting
something to a variable, checking its state, etc., to achieve some  behavior and
then wrap it around and interface, like a function. Functional  programming uses
higher order function to abstract away common patterns, like examining two lists
in pairs and doing something with those pairs or  getting a set of solutions and
eliminating the ones you don't need.
We'll implement another function that is already in the standard library, called
flip. Flip simply takes a function and returns a function  that is like our ori-
ginal function, only the first two  arguments are flipped.  We  can implement it
like so:

> flip' :: (a -> b -> c) -> (b -> a -> c)
> flip' f = g
>     where
>     g x y = f y x

Reading the type declaration, we say that it takes a function  that takes an 'a'
and a 'b' and returns a function that takes a 'b' and an 'a'. But  because func-
tions are curried by default, the second pair of parentheses is  really unneces-
sary, because → is right associative by default. So to be really clear:
                   (a → b → c) → (b → a → c) 
Which is the same as:
                   (a → b → c) → (b → (a → c))
Which also is the same as:
                   (a → b → c) → b → a → c
We wrote that g x y = f y x. If that's true, then f y x = g x y  must also hold,
right? Keeping that in mind, we can define this  function in an even simpler ma-
nner.

> flip'' :: (a -> b -> c) -> b -> a -> c
> flip'' f y x = f x y

Here, we take  advantage of  the fact that  functions are curried.  When we call
flip'' without the parameters y and x, it will return an f that takes  those two
parameters but  calls them  flipped. Even though  flipped functions are  usually
passed to other function, we can takee advantage of currying  when making higher
order functions by thinking ahead and writing what their end result  would be if
they were called fully applied.

Prelude> flip'' zip [1,2,3,4,5] "hello"
[('h',1),('e',2),('l',3),('l',4),('o',5)]
Prelude> zipWith (flip'' div) [2,2..] [10,8,6,4,2]
[5,4,3,2,1]

