Chapter Exercises
--------------------------------------------------------------------------------
First let's define these expression to work with them in the exercises:

> awesome     = ["Papuchon", "curry", ":)"]
> alsoAwesome = ["Quake", "The Simons"]
> allAwesome  = [awesome, alsoAwesome]

length is a function that  takes a list and returns a result that tells how many 
items are in a list:

1. Given the definition of "length" above, what would the type signature be? How
   many  arguments, of what type does it take? What is the of the result it eva-
   luates to?
     * The type signature will be: "length :: Foldable t => t a -> Int"
     * It will take one argument, a list of a type variable 'a'
     * It will evaluate to an Int

2. What are the results of the following expressions?
  
  Prelude> length [1, 2, 3, 4, 5]
  5
  Prelude> length [(1, 2), (2, 3), (3, 4)]
  3
  Prelude> length allAwesome
  2
  Prelude> length (concat allAwesome)
  5

3. Given what we know about numeric types and the type signature of length, look
   at these two expressions. One works and one returns an error. Determine which
   will return an error and why.

   Prelude> 6 / 3
   2.0
   Prelude> 6 / length [1, 2, 3]
   <interactive>:8:1: error:
       * No instance for (Fractional Int) arising from a use of `/'
       * In the expression: 6 / length [1, 2, 3]
         In an equation for `it': it = 6 / length [1, 2, 3]
   
   No problem in the first one. The second one will present the problem of frac-
   tional division between 6 (which in this context GHCi will assume its a float
   given that  we are trying to  perform fractional  division) and the result of
   the  expression  "length [1, 2, 3]" which will be an Int. There are two solu-
   tions to these issue:

   Prelude> 6 `div` length [1, 2, 3]
   2
   Prelude> 6 / fromIntegral (length[1, 2, 3])
   2.0

4. How can you fix the broken code from the preceding exercise using a different
   function / operator?

5. What is the type  of the  expression "2 + 3 == 5"?  What would we expect as a
   result?

   Prelude> :type 2 + 3 == 5
   2 + 3 == 5 :: Bool
   Prelude> 2 + 3 == 5
   True
   
6. What is the type and expected result value of the following:

   Prelude> let x = 5
   Prelude> :type x
   x :: Num a -> a
   Prelude> :type x + 3 == 5
   x + 3 == 5 :: Bool
   Prelude> x + 3 == 5
   False

7. Below  are some  bits of code. Which will  work? Why or why not? If they will
   work, what value would these reduce to?

   Prelude> length allAwesome == 2  
   -- Will work.
   Prelude> length [1,'a',3,'b'] 
   -- Will not work. Elements of different types.
   Prelude> length allAwesome + length awesome 
   -- Will work. Both are of type Int.
   Prelude> (8 == 8) && ('b' < 'a') 
   -- Will work. Both are of type Bool.
   Prelude> (8 == 8) && 9 
   -- Will not work. Doing and "and" between a Bool and a Num.
   
8. Write is a function that tells you whether or not a given String (or list) is
a palindrome. Here you will want to use a function called "reverse" a predefined
function that does just what is sounds like.

> isPalindrome :: (Eq a) => [a] -> Bool
> isPalindrome xs = xs == reverse xs

9. Write a function to return the absolute value of a number using if-then-else.

>  myAbs :: Integer -> Integer
>  myAbs x = if x >= 0 then x else x * (-1)

10. Fill in the definition of the following function, using fst and snd:

> f :: (a, b) -> (c, d) -> ((b, d), (a, c))
> f a b = ((snd a, snd b), (fst a, fst b))

Correcting Syntax
--------------------------------------------------------------------------------
In the following  examples, you will be shown syntactically incorrect code. Type
it in and try to correct it in your text editor, validating it with GHCi.

1. Here, we want a function  that adds 1 to the length  of a string argument and
   returns that result.

   x = (+)
   F xs = w 'x' 1
       where
           w = length xs
   
   Solution: 
   x = (+)
   f xs = x w 1
       where
       w = length xs

2. This is supposed to be the identity function, id:

   \ X = x
   
   Solution:
   \x -> x

3. When fixed, this function will return 1 from the value [1, 2, 3]. 

   \ x : xs -> x
   
   Solution:
   \(x:xs)->x

4. When fixed, this function will return 1 from the value (1, 2).

   f (a b) = A

   Solution:
   f (a, b) = a
   
Match the function names to their types
--------------------------------------------------------------------------------
1. Which of the following types is the type of show?

   c) Show a => a -> String
   
2. Which of the following types is the type of (==)?

   b) Eq a => a -> a -> Bool
   
3. Which of the following types is the type of fst?

   a) (a, b) -> a

4. Which of the following types is the type of (+)?

   d) Num a => a -> a -> a
   
