Haskell Guards
--------------------------------------------------------------------------------
2017, Ignacio Matías Sniechowski

Haskell programs  often use boolean operators in convenient and abbreviated syn-
tax. When the  same logic is  written in alternative  styles, we call this "syn-
tactic sugar"  because it sweetens the  code from the human perspective. We will
start with guards,a feature that relies on boolean values and allows us to write
simple but powerful functions. 

Let's implement the absolute value function. The absolute value of a real number
is the number with its sign discarded; so if the number is negative  the sign is
inverted; otherwise it remains unchanged. To  express this  decision  process in
Haskell using guards, the implementation could look like this:

> absolute :: (Num a, Ord a) => a -> a
> absolute x
>    | x < 0      = (-x)
>    | otherwise  =   x

Remarkably,the above code is very readable. This function is already provided by
the Haskell Prelude with the name abs. Let's dissect the components of the defi-
nition:

* We start just like a normal function definition,providing a name for the func-
  tion (.i.e.,absolute) and saying it will take a single argument, which we will
  name x.
* Instead of just following with the = and the right-hand side of the definition
  ,we enter the two alternatives placed below on  separate lines. These alterna-
  tives are the guards proper. Note that the whitespace -the  indentation of the
  third and fourth lines- is not just for aesthetic reasons; it is necessary for
  the code to be parsed correctly.
* Each of the guards begins with a pipe character |. After the pipe, we  put  an
  expression which evaluates to a boolean (also called a  boolean  condition  or
  predicate), which is followed by the rest of the definition. The function only
  uses the equals sign and the right-hand side from a line if the predicate eva-
  luates to True.
* The otherwise case is used when  none of the preceding  predicates evaluate to
  True. In this case, if x is not smaller than zero, it  must be greater than or
  equal to zero, so the final predicate could have just as easily been x>=0; but
  otherwise works just as well.

There is no magic behind otherwise. It is defined in the Prelude simply as:

otherwise = True

This definition makes otherwise a catch-all guard. As  evaluation of  the  guard
predicates is sequential, the otherwise predicate  will only be  reached if none
of the previous cases evaluate to True -so make sure you always  place otherwise
as the last guard. In general, it is a good  idea to always provide an otherwise
guard, because a rather ugly runtime error will be  produced if none of the pre-
dicates is true for some input.

where and Guards
--------------------------------------------------------------------------------
where clauses work well with guards.  For instance,  consider a  function  which
computes the number of real solutions for a quadratic equation: ax^2 + bx + c =0
 
> numOfQuadraticRoots :: (Ord a, Num a, Num b) => a -> a -> a -> b
> numOfQuadraticRoots a b c
>     | discriminant >  0  = 2
>     | discriminant == 0  = 1
>     | otherwise          = 0
>        where
>        discriminant = b ^ 2 - 4 * a * c

The where definition is within the scope of all  of the guards,  sparing us from
repeating the expression for discriminant.

* Exercise: Write  a function  that computes the real roots of a quadratic equa-
  tion.

> quadraticRoots :: (Ord a, Fractional a) => a -> a -> a -> (a, a)
> quadraticRoots a b c | discrim >= 0  = calculateRoots a b c
>                      | otherwise     = error "This equation has no real roots"
>             where
>             discrim           = b ^ 2 - 4 * a * c
>             calculateRoots a b c = ( ((-b) + discrim) / 2, (b + discrim) / 2 )

Example:

quadraticRoots 1 2 0  -- which is x ^ 2 + 2x = 0
(1.0,3.0)

quadraticRoots 1 2 3  -- which is x ^ 2 + 2x + 3 = 0
*** Exception: This equation has no real roots

