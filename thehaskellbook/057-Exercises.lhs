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
   
   
    


