Conditionals with if-then-else
--------------------------------------------------------------------------------
Haskell does not have "if" statements but it does have if *expressions*. It is a
built-in of syntax that works with the Bool datatype.

Prelude> if True then "this is the truth" else "this is false!"
"this is the truth"

Prelude> if False then "this is the truth" else "this is false!"
"this is false!"

Prelude> :type if True then "true!" else "false!"
if True then "true!" else "false!" :: [Char]

The structure here is: 

if CONDITION
then EXPRESSION_A
else EXPRESSION_B

If the CONDITION (which must evaluate to Bool) reduces to the  Bool  value True,
then EXPRESION_A is the result, otherwise EXPRESSION_B. In the above example the
type was String (or [Char]) because that is the type of the value that is retur-
ned as a result.
If-expressions can be thought  of as a way to choose between two values. You can
embed a variety of expressions within  the if of an if-then-else, as  long as it
evaluates to Bool.Also,the types of the expressions in the then and else clauses
must be the same.

Prelude> let x = 0
Prelude> if (x + 1 == 1) then "AWESOME" else "what?"
"AWESOME"

And this is how it reduces:

1. x = 0
   if (x + 1 == 1) then "AWESOME" else "what?"
2. if (0 + 1 == 1) then "AWESOME" else "what?"
3. if (    1 == 1) then "AWESOME" else "what?"
4. if True then "AWESOME" else "what?"
4. "AWESOME"

But this does not work:

Prelude> let x = 0
Prelude> if (x * 100) then "adopt a dog" else "or a cat"

<interactive>:10:5: error:
    * No instance for (Num Bool) arising from a use of `*'
    * In the expression: (x * 100)
      In the expression: if (x * 100) then "adopt a dog" else "or a cat"
      In an equation for `it':
          it = if (x * 100) then "adopt a dog" else "or a cat"

We got this type error because the condition passed to the if  expression is  of
type  "Num a ⇒  a", not Bool  and Bool does not implement the Num type class. To
oversimplify, (x * 100) evaluates to a numeric result, and numbers are not truth
values.  Let us see an example of a function that uses a Bool value in an if ex-
pression:

> module GreetIfCool where
>
> greetIfCool :: String -> IO ()
> greetIfCool coolness =
>   if cool coolness
>     then putStrLn "What's up?"
>   else
>     putStrLn "Shhhh"
>   where
>   cool x = x == "do it right"

If you load this in GHCi:

Prelude> :load 044-Conditionals.lhs
[1 of 1] Compiling GreetIfCool      ( 044-Conditionals.lhs, interpreted )
Ok, modules loaded: GreetIfCool.
*GreetIfCool> greetIfCool "do it right"
What's up?
*GreetIfCool> greetIfCool "hello"
Shhhh

