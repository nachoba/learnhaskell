Chapter Remarks
--------------------------------------------------------------------------------
1. A tuple is an ordered grouping of values. In Haskell, you cannot have a tuple
   with only one element, but there is a zero tuple also called unit or ().  The
   types of the elements of tuples are allowed to vary, so for instance, you can
   have both (String, String) or (Integer, String). Tuples in Haskell are the u-
   sual means of expressing an anonymous product.
2. A typeclass is a set of operations defined with respect to a polymorphic type
   When a type is an instance of a typeclass, values of that type can be used in
   the standard operations defined for that typeclass.   In Haskell, typeclasses
   are unique pairings of class and concrete instance.This means that if a given
   type a has an instance of Eq, it has only one instance of Eq.
3. Data constructors in Haskell provide a means of creating values that inhabit
   a given type.Data constructors in Haskell have a type and can either be cons-
   tant values (nullary) or take one or more arguments just like functions.   In
   the following example, Cat is a nullary data constructor for Pet and Dog is a
   data constructor that takes an argument:

> type Name = String
> 
> data Pet = Cat | Dog Name

   The data constructors have the following types:
                 Prelude> :t Cat
                 Cat :: Pet
                 Prelude> :t Dog
                 Dog :: Name → Pet
4. Type constructors in Haskell are not values and can only be used in type sig-
   natures.Just as data declarations generate data constructors to create values
   that inhabit that type,data declarations generate type constructors which can
   be used to denote that type. In the above example Pet is the type constructor
   A  guideline for  differentiating the two kinds of  constructors is that type
   constructors always go to the left of the "=" in a data declaration.
5. Data declarations define new datatypes in  Haskell.  Data declarations always
   create new data constructors,but may or may not create new data constructors.
   Data declarations are how we refer to the entire  definition that beings with
   the data keyword.
6. A type  alias is a way to refer to a type constructor or type constant  by an
   alternate name,usually to communicate something more specific of for brevity.

> type Nombre = String

   This creates a new type alias Nombre of the type String and not a data decla-
   ration, just a type alias declaration.
7. Arity is the number of arguments a function accepts.  This notion is a little
   slippery in  Haskell as, due to  currying, all  functions  are 1-arity and we
   handle accepting multiple arguments by neting functions.
9. Polymorphism  in Haskell  means  being able to  write code in terms of values
   which may be one of several, or any, type. Polymorphism in Haskell  is either
   parametric or constrained.The identity function, id, is an example of a para-
   metrically polymorphic function:
                 id :: a → a
                 id x = x
   Here id works for a value of any type because it does not use any information
   specific to a given type or set of types.   Whereas,  the following  function
   isEqual:
                 isEqual :: Eq a ⇒ a → a → Bool
                 isEqual x y =  x == y
   Is polymorphic, but constrained or bounded to the set of types which have  an
   instance of the Eq typeclass.The different kinds of polymorphism will be dis-
   cussed in greater detail in a later chapter.

Names and variables
--------------------------------------------------------------------------------
Names: In Haskell there are seven categories of entities that have names:
                 * functions
                 * term-level variables
                 * data constructors
                 * type variables
                 * type constructors
                 * typeclasses
                 * modules
       Term-level variables and data constructors exist in your terms.Term-level
       is where your values live and is the code that executes when your program
       is running. At the type-level, which is used during the static analysis &
       verification  of your program, we have type variables, type constructors,
       and typeclasses. Lastly,for the purpose of organizing our code into cohe-
       rent groupings across different files (more on this later), we have modu-
       les.

Convention for variables: Haskell uses a lot of variables,  and some conventions
       have developed. It's not critical that you memorize this, because for the
       most part, these are merely conventions,  but familiarizing yourself with
       them will help you read Haskell code.Type variables (that is variables in
       type signatures) generally start at a and go from there:  a, b, c, and so 
       forth. You may occasionally see them with numbers appended to them: a1.
       Functions can be used as arguments and in that case are typically labeled
       with variables starting at f, followed by g and so on. They may sometimes
       have numbers appended: f1,and also sometimes decorated with the ' charac-
       ter as in f'.This would be pronounced "eff-prime", should you need to say
       it aloud. Usually this denotes a function that is closely related to or a
       helper function to function f. Functions may also be given variable names
       that are not on this spectrum as a mnemonic. For example, a function that
       results in a list of prime numbers might  be called p, or a function that
       fetches some text might be called txt.
       Variables do not have to be a single letter. In small programs,they often
       are; in larger programs, they are often not a single letter. If there are
       many variables in a function or program,  as is common,  it is helpful to
       have descriptive variable names. It is often advisable in domain-specific
       code to use domain-specific variable names.
       Arguments to functions are most often given names starting at x,again oc-
       casionally seen numbered as in x1. Other single-letter variable names may
       be chosen when they serve a mnemonic role,such as choosing r to represent
       a value  that is the radius of a circle. If you have a list of things you
       have named x,  by convention that will usually be called xs, that is, the
       plural of x. You will see this convention often in the form (x:xs), which
       means you have  a list in which the head of the list is x and the rest of
       the list is xs.
       All of these, though, are merely conventions, not definite rules.While we
       will generally adhere to the  conventions in this book,  any Haskell code
       you see out in the wild may not.Calling a type variable x instead of a is
       not going to break  anything. As in the lambda calculus, the names do not
       have any inherent  meaning.  We offer this  information as a  descriptive
       guide of Haskell conventions,not as rules you must follow in your own co-
       de.

