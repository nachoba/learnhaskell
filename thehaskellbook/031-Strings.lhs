Chapter 3: Strings in Haskell
-------------------------------------------------------------------------------
Most programming languages refer to the data structures used to contain text as
"strings", represented as sequences, or lists, of characters. Haskell is no ex-
ception.

A first look at types
---------------------
Types are important in Haskell. Types are a way of categorizing values. There're
several types  for numbers (integers, fractional numbers, etc).  There is a type
for boolean values (True and False are types of Bool), and  also there are types
concerned  with characters, Char, and strings, String. Strings are lists of cha-
racters.

It is easy  to find out the type of a value, expression, or function in GHCi. We
do this with the ":type" command. In the REPL type:

Prelude> :type 'a'
'a' :: Char

1. We enclosed our character in single quotes, this lets GHCi know that the cha-
   racter is not a variable. If we enter:

   Prelude> :type a
   Not in scope: ‘a’

   That is, the variable a hasn't been defined (it is not in scope), so GHCi has
   no way to know what the type of it is.
2. The :: symbol is read as  "has the type". Whenever you see that double colon,
   you know you are looking at a type  signature. A  type signature is a line of
   code that defines the types for a value, expression, or function.
3. Finally there is Char, the type. Char is a type that includes alphabetic cha-
   racters, unicode characters, symbols, etc.

Now let us try a string of text. This time we have to use double quotation marks
and not single quotations marks like in chars.

Prelude> :type "Hello!"
"Hello!" :: [Char]

The square  brackets around Char here are the syntactic sugar for a list. String
is a  "type alias", or type synonym, for a list of Char. A type alias is what it
sounds like:  we use one name for a  type, that has a different type name under-
neath. So here, String is just another name for a list of characters, or [Char].

Printing simple strings
-----------------------
Let's see how to print strings of text in the REPL:

Prelude> print "hello world!"
"hello world!"

Prelude> putStrLn "hello world!"
hello world!

It  seems that both, print and putStrLn are similar to each other,  with one key
difference, the quotation marks. This happens because while they are superficia-
lly similar, they have different types.

Prelude> :type print
print :: Show a ⇒ a → IO ()

Prelude> :type putStrLn
putStrLn :: String → IO ()

Now let's see how to do these things from source files:

> main :: IO ()
> main = do
>   putStrLn "Hello Word!"
>   putStrLn "Hello Nacho!"

main is the default action when we build an executable or run it in the REPL. It
is not a function but  is often a series of  instructions to  execute, which can 
include applying functions and producing side-effects. As you can see, main  has
type IO (). IO stands for Input/Output but has a specialized meaning in Haskell.
It is a special type  used when the  result of running the program involves side 
effects  in addtion to being a function or expression. Printing to the screen is
an effect, so printing the output of a module must be wrapped in this IO type.

The do syntax allows for sequencing actions. It's most commonly used to sequence
the actions that constitute your program, some of which will necessarily perform
effects (such as printing to the screen). The do is not  strictly necessary, but 
since it often makes for more readable  code than the alternatives, you will see
it a lot.
