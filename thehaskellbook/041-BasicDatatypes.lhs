Basic Datatypes
-------------------------------------------------------------------------------

Introduction
Haskell has a  robust and expressive type system. Types play  an important role
in the readability, safety, and  maintainability of  Haskell code as they allow
us to classify  and delimit data, thus  restricting the  forms of data our pro-
grams can  process.  Types, also called datatypes,  provide  the means to build
programs  more quickly  and also  allow for greater ease  of maintenance. As we
learn more  Haskell, we will learn  how to leverage types in a way that lets us
accomplish the same things but with less code.

What are Types?
Expressions, when  evaluated, reduce  to values. Every  value has a type. Types 
are how we group a set of values together that share something in common. Some-
times that  "something in common"  is abstract, sometimes it's a specific model
of a particular concept or domain.

Anatomy of a data delcaration
Data declarations are how datatypes are defined.The type constructor is the na-
me of the  type and is capitalized. Data constructors are the values that inha-
bit the type they are defined in. They are the values that show up in your code
,at the term level instead of the type level. By "term level", we mean they are
the  values as they  appear in your code or the values that your code evaluates
to.
We will start with a basic datatype to see how datatypes are structured and get
acquainted with the vocabulary.

data Bool = False | True

[1] Bool  : Type constructor for datatype Bool.This is the name of the type and
            shows up in type signatures. 
[2] False : Data constructor for the value False. 
[3]  |    : Pipe indicates a sum type, or  logical disjunction: "or". So a Bool 
            value is True or False. 
[4] True  : Data constructor for the value True.

The whole thing is called a data  declaration. Data declarations  do not always 
follow precisely the same pattern.There are datatypes that use logical conjunc-
tion "and" instead of disjunction,and some type constructors and data construc-
tors  may have arguments. The thing they have in common is the keyword data fo-
llowed by the type  constructor  (or name of the type that  will appear in type
signatures), the equals sign to denote a definition, and the data constructors.

You can find the datatype definition for built-in datatypes by using:

Prelude> :info Bool 
data Bool = False | True

Let us look at where  different parts of  datatypes show up  in our code. If we 
query the type information for a function called  not, we see that it takes one 
Bool value and returns another Bool value,so the type signature makes reference
to the type constructor, or datatype name:

Prelude> :type not 
not :: Bool → Bool

But when we use the not function, we use the data constructors, or values:

Prelude> not True
False

And our expressions evaluates to another data constructor, or value.

> module Datatype where
>
> data Mood = Blah | Woot
>             deriving Show
>
> changeMood :: Mood -> Mood
> changeMood Woot = Blah
> changeMood    _ = Woot
>
> main :: IO ()
> main = print (changeMood Woot)


