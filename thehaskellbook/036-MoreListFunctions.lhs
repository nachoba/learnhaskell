More list functions
--------------------------------------------------------------------------------
Since  a String is a  specialized type of list, you can use standard list opera-
tions on strings as well.

Prelude> :type 'c'
'c' :: Char

Prelude> :type "c"
"c" :: [Char]

Now we will see some operators:
-------------------------------
* cons      : The : operator is called "cons", builds a list.
* head      : Returns the head or first element of a list.
* tail      : Returns the list with the head chopped off.
* take n    : Returns  the specified  number of elements from the list, starting
              from the left.
* drop n    : Returns the remainder of the list after the specified number of e-
              lements has been dropped.
* ++        : Concatenates two lists.
* !!        : Returns the element that is in the specified position in the list.
              Note  that this is  an indexing  function, and  indices in Haskell
              start from 0.

> module StringFunctions where
>
> main :: IO ()
> main = do
>   print ( 'N' : "acho" )
>   print ( head "Nacho" )
>   print ( tail "Nacho" )
>   print ( take 2 "Nacho" )
>   print ( drop 2 "Nacho" )
>   print ( "Hello" ++ " " ++ "Nacho" )
>   print ( "Nacho" !! 1)

The output of this program is:
'N' : "acho"              => "Nacho"
head "Nacho"              => 'N'
tail "Nacho"              => "acho"
take 2 "Nacho"            => "Na"
drop 2 "Nacho"            => "cho"
"Hello" ++ " " ++ "Nacho" => "Hello Nacho"
"Nacho" !! 1              => 'a'

Note that while all these functions are standard Prelude functions, many of them
are not considered safe.They are unsafe because they do not cover the case where
they are given an empty list as input. Instead they just throw out an error mes-
sage, or exception.

Prelude> head ""
*** Exception: Prelude.head: empty list

Prelude> "" !! 4
*** Exception: Prelude.!!: index too large

This is not ideal  behavior, so the use  of these functions is considered unwise
for programs of any real size or complexity.
