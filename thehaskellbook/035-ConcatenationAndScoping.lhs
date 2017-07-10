Concatenation and scoping
--------------------------------------------------------------------------------
We will use parentheses to call ++ as prefix (not infix) function:

> module PrintFlipped where
>
> myGreeting :: String
> myGreeting = (++) "hello" " world!"
> 
> hello :: String
> hello = "hello"
>
> world :: String
> world = "world!"
>
> main :: IO ()
> main = do
>   putStrLn myGreeting
>   putStrLn secondGreeting
>    where
>    secondGreeting = (++) hello ( (++) " " world)

In secondGreeting, using ++ as prefix function forces us to shift some things a-
round.  Parenthesizing it that way  emphasizes the right associativity of the ++
function. Since it's an infix operator, we can check it's right associative:

Prelude> :info (++)
(++) :: [a] → [a] → [a] 	-- Defined in ‘GHC.Base’
infixr 5 ++

The where clause creates local bindings  for expressions that are not visible at
the top level. In other words, the where clause in the main function  introduces
a definition visible only within  the expression of  function it is attached to,
rather than making it visible to the entire module. Something visible at the top
level is in scope for all parts of the  module and may be exported by the module
or imported by a different module.  Local definitions  are only  visible to that
function; you cannot import into a different module and reuse.
