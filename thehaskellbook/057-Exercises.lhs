Chapter Exercises
--------------------------------------------------------------------------------
Welcome to another round of "Knowing is not enough; we must apply"

Multiple Choice
--------------------------------------------------------------------------------
1. A value of type [a] is:
   c) a list whose elements are all of some type a
2. A function of type [[a]] → [a] could
   a) take a list of strings as an argument
3. A function of type [a] → Int → a
   b) returns one element of type a from a list
4. a function of type (a,b) → a
   c) takes a tuple argument and returns the first value


Determine the type
--------------------------------------------------------------------------------
For the following functions,  determine the type of the specified value. We sug-
gest you type them into a file and load the contents of the file in GHCi. In all
likehood,  it initially will not have the polymorphic types you might expect due 
to the monomorphism restriction. We will explain more much later, but for now it
means  that top-level declarations  by default will have a concrete  type if any 
can be determined. You can fix this by setting up your file like so:

> {-# LANGUAGE NoMonomorphismRestriction #-}
>
> module DetermineTheType where
>
> example = 1

If you had not included the NoMonomorphismRestriction pragma, example would have
the type Integer instead of Num a ⇒ a. Do your best to determine  the most poly-
morphic type an expression could have in the following exercises.
1. All function application return a value.Determine the value returned by these
   function applications and the type of that value.
   (a) (* 9) 6
       54
       54 :: Num a ⇒ a

   (b) head [(0, "doge"), (1, "kitteh")]
       (0, "doge")
       (0, "doge") :: Num a ⇒ (a, [Char])

   (c) head [(0 :: Integer, "doge"), (1,"kitteh")]
       (0, "doge")
       (0, "doge") :: (Integer, [Char])

   (d) length [1, 2, 3, 4, 5]
       5
       length [1,2,3,4,5] :: Int

   (e) (length [1, 2, 3, 4]) > (length "TACOCAT")
       False
       (length [1, 2, 3, 4]) > (length "TACOCAT") :: Bool

2. Given:
   x = 5
   y = x + 5
   w = y * 10

   What is the type of w?
   w :: Num a ⇒ a

3. Given:
   x = 5
   y = x + 5
   z y = y * 10

   What is the type of z?
   z :: Num a ⇒ a → a

4. Given:
   x = 5
   y = x + 5
   f = 4 / y

   What is the type of f?
   f :: Fractional a ⇒ a

5. Given:
   x = "Julie"
   y = " <3 "
   z = "Haskell"
   f = x ++ y ++ z

   What is the type of f?
   f :: [Char]

Does it compile?
--------------------------------------------------------------------------------
For each set of expressions, figure out which expression, if any,causes the com-
piler to squawk at you and why. Fix it if you can.

1. bigNum = (^) 5 $ 10
   wahoo  = bigNum $ 10


2. x = print
   y = print "woohoo!"
   z = x "hello world!"
   
3. a = (+)
   b = 5
   c = b 10
   d = c 200

4. a = 12 + b
   b = 10000 * c

Type variable or specific type constructor?
--------------------------------------------------------------------------------
1. You will be shown a type declaration, and you should categorize each type.The
   choices are a  fully polymorphic type  variable, constrained polymorphic type
   variable, or concrete type constructor.
   f :: Num a ⇒ a → b → Int → Int
               [0] [1]  [2]   [3]

   Here, the answer would be:
   [0]. Constrained polymorphic (Num).
   [1]. Fully polymorphic.
   [2]. Concrete.
   [3]. Concrete.

2. Categorize  each component of the type signature as described in the previous
   example.
   f :: zed → Zed → Blah

3. Categorize each component of the type signature.
   f :: Enum b ⇒ a → b → C

4. Categorize each component of the type signature.
   f :: f → g → C

Write a type signature
--------------------------------------------------------------------------------
For the following expressions, please add a type signature.You should be able to 
rely on GHCi type inference to check your work, although you might not have pre-
cisely the same answer as GHCi gives (due to polymorphism, etc).

1. While we haven't fully explained this syntax yet,you've seen it in a previous
   chapter. This syntax is a way of destructuring a single element of a list.

   (a). functionH ::
        functionH (x:_) = x

   (b). functionC ::
        functionC x y = if (x > y) then True else False

   (c). functionS ::
        functionS (x,y) = y

Given a type, write the function
--------------------------------------------------------------------------------
You will be shown a type and a function that needs to be written. Use the infor-
mation the type provides to determine what the function should do.  We will also
tell you how many  ways there are to write the function. Syntactically different
but semantically equivalent,writing a function one way then rewriting the seman-
tically identical  function but using anonymous lambda syntax  does not count as
two implementations. To make things a little easier, we will  demonstrate how to
solve this kind of exercise. Given:

myFunc :: (x → y)
        → (y → z)
        → c
        → (a, x)
        → (a, z)
myFunc xToY yToZ _ (a,x) = undefined

Talking through the above,we have a function that takes four arguments.The final
result is a tuple with the type (a,z).It turns out, the c argument is nowhere in
our results and there's nothing really to do with it,so we use the underscore to
ignore that. We named the  two function arguments by their types and patter mat-
ched on the tuple argument.  The only way to  get the second value of the  tuple
from the type x to the type z is to use both of the functions furnished to us.If
we tried the following:

myFunc xToY yToZ _ (a, x) = (a, (xToY x))

We would get a type error that it expected the type z but the actual type was y.
That's because we're on the right path, but not quite done yet! Accordingly, the
following should typecheck:

myFunc :: ( x → y )
        → ( x → z )
        →   c
        → ( a , x )
        → ( a , z )
myFunc xToY yToZ _ (a, x) = (a, (yToZ ( xToY x )))

1. There is only one function definition that typechecks and doesn't go into  an
   infinite loop when you run it.
   i :: a → a
   i = undefined

2. There is only one version that works.
   c :: a → b → a
   c = undefined

3. Given alpha equivalence are c'' and c the same thing?
   c'' :: b → a → b
   c'' = ?

4. Only one version that works.
   c' :: a → b → b
   c' = undefined

5. There are multiple possibilities, at least two of which you have seen in pre-
   vious chapters.
   f :: [a] → [a]
   f = undefined

6. Only one version that will typecheck.
   co :: (b → c) → (a → b) → a → c
   co = undefined

7. One version will typecheck.
   a :: (a → c) → a → a
   a = undefined

8. One version will typecheck.
   a' :: (a → b) → a → b
   a' = undefined

Fix It
--------------------------------------------------------------------------------
Won't someone take pity on this poor broken code and fix it up? Be sure to check
carefully for things like capitalizacion, parentheses, and indentation.
    


