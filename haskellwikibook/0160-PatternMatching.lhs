Pattern matching
--------------------------------------------------------------------------------
2017, Ignacio Matías Sniechowski

In pattern matching, we attempt to match values against patterns and,if so desi-
red, bind variables to succesful matches.

Analysing pattern matching
--------------------------------------------------------------------------------
Pattern matching  is virtually everywhere. For example, consider this definition
of map:
                                     map []         = []
                                     map f (x : xs) = f x : map f xs

At surface level, there are four different patterns involved, two per equation.

* "f" is a pattern that matches  anything at all, and binds  the "f" variable to
  whatever is matched.
* "(x:xs)" is a  pattern that  matches a non-empty list which is formed by some-
  thing -which gets bound to the "x" variable- which  was cons'd -using  the (:)
  operator- onto something else -which gets bound to "xs".
* "[]" is a pattern that matches the empty list.  It doesn't bind any variables.
* "_" is the  pattern which  matches anything  without binding -wildcard, "don't
  care" pattern.

In the "(x:xs)" pattern, "x" and "xs" can be seen as sub-patterns  used to match
the parts of the list. Just like "f", they match  anything -though it is evident
that if there is a successful match and "x" has type a, "xs" wil have  type [a].
Finally, these considerations imply that "xs" will also match an empty list, and
so a one-element list matches "(x:xs)".From the above dissection,we can say pat-
tern matching gives us a way to:

* recognize values: For instance,when map is called and the second argument mat-
  ches [] the first equation for map is used instead of the second one.
* bind variables to the recognized values. In this case, the variables "f", "x",
  and "xs" are assigned to the values passed as arguments to map when the second
  equation  is used, and so we can use these  values passed as  arguments to map
  when the second equation is used, and  so we can use these values  through the
  variables in the right-hand side of =. As _ and [] show, binding is not an es-
  sential part  of pattern  matching, but just a side effect of  using  variable
  names as patterns.
* break down values into parts, as the (x:xs) pattern does by binding two varia-
  bles parts (head and tail) of a matched argument (the non-empty list).

The connection with constructors
--------------------------------------------------------------------------------
Despite the detailed analysis above, it may seem too magical how we break down a
list as if we were undoing the effects of the (:) operator.Be careful: this pro-
cess will not work with any arbitrary operator.  For example, one might think of
defining a function  which uses (++) to  chop off the  first three elements of a
list:
                                                  dropThree ([x,y,z] ++ xs) = xs

But that will not work. The function (++) is  not allowed  in patterns. In fact,
most other function that act on list are similarly  prohibited from pattern mat-
ching.  Which functions, then, are allowed? In one word, constructors -the func-
tions used to build values of algebraic data types.Let's consider a random exam-
ple:
                                                          data Foo=Bar | Baz Int

Here Bar and Baz are constructors for the type Foo.You can use them  for pattern
matching Foo values and bind variables to the Int value contained in a Foo cons-
tructed with Baz:

> data Foo = Bar | Baz Int
>
> f :: Foo -> Int
> f Bar     = 1
> f (Baz x) = x - 1

This is exactly like showAnniversary and showDate in the Type Declarations modu-
le. For instance:

> data Date = Date Int Int Int
>
> showDate :: Date -> String
> showDate (Date y m d) = show y ++ "-" ++ show m ++ "-" ++ show d

The (Date y m d) pattern in the lefthand side of the showDate definition matches
a Date -built with the Date constructor- and binds the variables y, m, and d  to
the contents of the Date value.

