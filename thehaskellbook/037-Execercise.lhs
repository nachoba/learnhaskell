Exercises Chapter 3
--------------------------------------------------------------------------------

> module ExercisesChapter03 where

Given the  list manipulation  functions mentioned  in this chapter, write  func-
tions that take the following inputs and return the expected outputs.

> string :: String
> string = "Curry is awesome"

[Exercise 1]--------------------------------------------------------------------
a) Given "Curry is awesome" → "Curry is awesome!"

> shout :: String -> String
> shout x = x ++ "!"

b) Given "Curry is awesome" → "y"

> get4 :: String -> Char
> get4 x = x !! 4

c) Given "Curry is awesome" → "awesome"

> drop9 :: String -> String
> drop9 x = drop 9 x

[Exercise 3]--------------------------------------------------------------------
Write a  function of type String → Char which  returns the third character in a
String.

> thirdLetter :: String -> Char
> thirdLetter x = x !! 2

[Exercise 4]--------------------------------------------------------------------
Now change  that function so the string  operated on is always the same, and the
variable represents the number of the letter you want to return.

> letterIndex :: Int -> Char
> letterIndex x = string !! x

[Exercise 5]--------------------------------------------------------------------
Using  the  take and drop functions  we looked at, write a function called rvrs.
That function  should take the  string "Curry is awesome" and return as a result
"awesome is Curry".

> rvrs :: String -> String
> rvrs x = drop 9 x ++ take 4 (drop 5 x) ++ take 5 x

--------------------------------------------------------------------------------
And now let's define a main function so we can test all the answers:


> main :: IO ()
> main = do
>   print "First Exercise"
>   print (shout string)
>   print (get4 string)
>   print (drop9 string)
>   print "Third Exercise"
>   print (thirdLetter string)
>   print "Fourth Exercise"
>   print (letterIndex 3)
>   print "Fifth Exercise"
>   print (rvrs string)

Running the main function in GHCi gives the following output:

"First Exercise"
"Curry is awesome!"
'y'
"awesome"
"Third Exercise"
'r'
"Fourth Exercise"
'r'
"Fifth Exercise"
"awesome is Curry"


Definitions of the Chapter
-------------------------------------------------------------------------------
1. A String is a sequence of characters. In Haskell, String is represented by a
   linked list of Char values: [Char]

2. A type or datatype  is a classification of  values or data. Types  in Haskell
   determine what  values are members of  the type or that inhabit the type. Un-
   like  other languges, datatypes in Haskell by default do not delimit the ope-
   rations that can be performed on that data.

3. Concatenation is  the joining together  of sequences of values. Often in Has-
   kell this is meant with respect to the [] or "List" datatype, which  also ap-
   plies  to  String which is [Char]. The  concatenation function  in Haskell is
   (++) which has type [a] -> [a] -> [a]. For example:

   Prelude> "tacos" ++ " " ++ "rock"
   "tacos rock"

4. Scope  is where a variable  referred to by name  is valid. Another  word used
   with the same meaning is visibility, because if a variable isn't visible it's
   not in scope.

5. Local bindings are  bindings local to particular expressions. The primary de-
   lineation  here from top-level bindings  is that local bindings cannot be im-
   ported by other programs or modules.

6. Top  level bindings in  Haskell are bindings that  stand outside of any other
   declaration. The main  feature of top-level bindings is that they can be made
   available to other modules within your programs or other people's programs.

7. Data  structures are a  way of organizing data so that the data can be acces-
   sed conveniently of efficiently.

