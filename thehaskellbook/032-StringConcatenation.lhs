String Concatenation
--------------------------------------------------------------------------------
To concatenate something means to link together. Usually when we talk about con-
catenation in programming we are talking about linear sequences such as lists or
strings of text.

> module Concatenate where
>
> myGreeting :: String
> myGreeting = "hello" ++ " world!"
>
> hello :: String
> hello = "hello"
>
> world :: String
> world = "world!"
>
> main :: IO ()
> main = do
>     putStrLn myGreeting
>     putStrLn secondGreeting
>       where
>      secondGreeting = concat [hello, " ", world]

Gives and output:

hello world!
hello world!

Remember that  String is a type  synonym for [Char]. This little program demons-
trate a few things:
1. We defined values at the top level of a module: myGreeting, hello, wolrd, and
   main. That is, they were declared at the top level so that they are available
   throughout the module.
2. We specify explicit types for top-level definitions.
3. We concatenate string with ++ and concat.
