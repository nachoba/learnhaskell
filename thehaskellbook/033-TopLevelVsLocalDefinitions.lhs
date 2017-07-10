Top-Level versus local definitions
--------------------------------------------------------------------------------
What does it mean for something to be at the top level of a module?  It does not
necessarily mean it's defined at the top of the file. Top-level declarations are
not nested within anything else and they are in scope throughout the module.
When the compiler reads the file, it will see all the top-level declarations, no
matter what order they come in the file.

We can contrast a top-level  definition  with a  local definition. To be locally 
defined would mean the declaration is nested within some other expression and is
not visible outside that expression.

> module TopOrLocal where
>
> topLevelFunction :: Integer -> Integer
> topLevelFunction x = x + woot + topLevelValue
>   where
>   woot :: Integer
>   woot = 10
>    
> topLevelValue :: Integer
> topLevelValue = 5

Prelude> topLevelFunction 6
21

In the above code, you could use  topLevelValue or topLevelFunction from another
module. They are accessible to everything else in  the  module. However, woot is 
effectively invisible outside of topLevelFunction. The where and let  clauses in
Haskell introduce local bindings or declarations.  To bind or declare  something
means to give an expression a name. It  would be possible to pass around and use
an anonymous version of topLevelFunction, but giving it a name and reusing it by
that name is less repetitious.

Also note that we have explicitly declared the type of woot in the where clause,
using the  :: syntax. This wasn't necessary as Haskell's type inference would've
figured it out, but it was done here to show you how to do it.
