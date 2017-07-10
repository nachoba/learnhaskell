Type Declarations
--------------------------------------------------------------------------------
2017, Ignacio Matías Sniechowski

You are not restricted to working with  just the types provided by default  with
the language. There are many benefits to defining your own types:

* Code can be written in terms of the problembeing solved,making programs easier
  to design, write and understand.
* Related  pieces of data  can be  brought together in ways  more convenient and
  meaninful than simply putting and getting values from lists of tuples.
* Pattern matching and the type system can  be used to  their fullest  extent by
  making them work with your custom types.

Haskell has three basic ways to declare a new type:

* The data declaration, which defines new data types.
* The type declaration for type synonyms,that is, alternative names for existing
  types -i.e., the String type which is an alias of [Char].
* The newtype declaration, which  defines new data  types equivalent to existing
  ones.

data and constructor functions
--------------------------------------------------------------------------------
data is used to  define new  data types mostly  using existing  ones as building
blocks. Here is a data structure for elements in a simple list of anniversaries:

> data Anniversary = Birthday String Int Int Int
>                  | Wedding  String String Int Int Int

This declares a new data type Anniversary, which can  be either a Birthday  or a
Wedding.A Birthday contains one string and three integers and a Wedding contains
two strings and three integers. The definitions of the two possibilities are se-
parated by the vertical bar |. Moreover, with the  declaration we  also get  two
"constructor functions" for Anniversary; appropriately  enough, they  are called
Birthday and Wedding. These functions provide a way to build a new Anniversary.
Types defined  by data declarations  are referred  to as "algebraic data types".
As usual with Haskell, the case of the first letter is important: type names and
constructor functions must start with capital letters. Other than this syntactic
detail, constructor functions work pretty much like "conventional" functions. In
fact, if you use ":type" in GHCi to query the type of, Birthday, you will get:

Prelude> :type Birthday
Birthday :: String → Int → Int → Int → Anniversary

Meaning it is just a function  which takes one String and three Int as arguments
and evaluates to an Anniversary.This anniversary will contain the four arguments
we passed as specified by the Birthday constructor. Calling  constructors  is no
different from calling other functions. For example, suppose we have:John Smith, 
born on 3rd July 1968

> johnSmith :: Anniversary
> johnSmith = Birthday "John Smith" 1968 7 3

He married Jane Smith, on 4th March 1987:

> smithWedding :: Anniversary
> smithWedding = Wedding "John Smith" "Jane Smith" 1987 3 4

These two anniversaries can, for instance, be put in a list:

> anniversariesOfJohnSmith :: [Anniversary]
> anniversariesOfJohnSmith =  [johnSmith, smithWedding]

Or you could easily have called the constructors straight away when building the
list -although the resulting code looks a bit cluttered.

> anniversariesOfJohnSmith' :: [Anniversary]
> anniversariesOfJohnSmith' = [ Birthday "John Smith" 1968 7 3
>                             , Wedding "John Smith" "Jane Smith" 1987 3 4
>                             ]

Deconstructing types
--------------------------------------------------------------------------------
To use  our new data type, we must have a way to access their contents. For ins-
tance, one very basic operation  with the anniversaries  defined above  would be
extracting the names and dates they contain as a String. 
So we need a showAnniversary function -for the sake  of code clarity, we  use an
auxiliary showDate function.

> showDate :: Int -> Int -> Int -> String
> showDate y m d = show y ++ "-" ++ show m ++ "-" ++ show d
>
> showAnniversary :: Anniversary -> String
> showAnniversary (Birthday name year month day) =
>             name ++ " born " ++ showDate year month day
>              
> showAnniversary (Wedding name1 name2 year month day) =
>             name1 ++ " married " ++ name2 ++ " on " ++ showDate year month day

This example shows  how we can  deconstruct the  values built in our data types.
showAnniversary takes a single argument of type Anniversary.Instead of just pro-
viding a name for the argument on the left  side of the definition,  however, we
specify one of the constructor functions and give names to  each argument of the
constructor -which correspond to the contents of the Anniversary.  A more formal
way of describing this "giving names" process is to say we're  binding variables.
"Binding" is being used in the sense of assigning a variable  to each of the va-
lues so that we can refer to them on the right side of the function definition.

To handle both Birthday and Wedding  anniversaries, we need to provide two func-
tion definitions, one for each  constructor. When showAnniversary is  called, if
the argument is a Birthday Anniversary, the first definition is used and the va-
riables name, month, date, and year are bound to its contents.  If the  argument
is a Wedding Anniversary, then the second  definition is used and  the variables
are bound in the same way.This process of using a different version of the func-
tion depending on  the type of the  constructor is pretty much like what happens
when we  use a case statement of define a function piece-wise. Note that the pa-
rentheses around  the constructor name and the bound variables are mandatory; o-
therwise the compiler or  interpreter would not take them  as a single argument.
Also, it is important to have it absolutely clear that the expression inside the
parentheses is not a call to the constructor  function, even though  it may look
just like one.

* Exercise: Reread the function definitions above. Then look closer at the help-
  er function showDate, there is a certain clumsiness in the way it is used. You
  have to pass three separate $Int$ arguments to it, but these arguments are al-
  ways linked to each other as part of a single date. It would  make no sense to
  do things like passing the year, month and day values of the Anniversary in  a
  different order, or to pass the month value  twice and  omit the day. Could we
  use what we have seen so far to reduce this clumsiness?
  Declare a Date  type which  is composed of  three Int, corresponding  to year,
  month and day.Then, rewrite showDate so that is uses the new $Date$ data type.
  What changes will then  be needed  in showAnniversary and  the Anniversary for
  them to make use of Date?

> data Date = Date Int Int Int
> data NewAnniversary = NewBirthday String Date
>                    | NewWedding  String String Date
>
> juanPerez :: NewAnniversary
> juanPerez =  NewBirthday "Juan Perez" (Date 1971 8 11)
>
> juanaPerez :: NewAnniversary
> juanaPerez =  NewWedding "Juan Perez" "Juana Perez" (Date 1997 3 4)
>
> showDate' :: Date -> String
> showDate' (Date y m d) = show y ++ "-" ++ show m ++ "-" ++ show d
>
> showAnniversary' :: NewAnniversary -> String
> showAnniversary' (NewBirthday name date)       =
>              name ++ " born " ++ showDate' date
> showAnniversary' (NewWedding name1 name2 date) =
>              name1 ++ " married " ++ name2 ++ " on " ++ showDate' date

Note that the Date type has a constructor function which is called Date as well.
That is perfectly valid and indeed giving the  constructor the  same name of the
type when there is just one constructor is good practice, as a simple way of ma-
king the role of the function obvious.

Type for making type synonyms
--------------------------------------------------------------------------------
As  mentioned in the introduction, code clarity is one of the motivations for u-
sing custom  types.  In that spirit,  it could be nice to make it clear that the
Strings in the Anniversary type are being  used as names  while still being able
to manipulate them like an ordinary String. This calls for a type declaration:

> type Name = String

The code above  says that a Name is now  a synonym, or  alias, for a String. Any
function that takes a String will now take a Name as well —and vice-versa: func-
tions that take Name will accept any String.The right hand side of a type decla-
ration can be a more complex type as well. For example, String itself is defined
in the Prelude as
                                                            type String = [Char]

We can do something similar for the list of anniversaries we made use of:

> type AnniversaryBook = [Anniversary']

Type synonyms are mostly just a convenience.  They help make the  roles of types
clearer or provide an alias to such things as complicated list or tuple types.It
is largely a matter of personal discretion to decide how type synonyms should be
deployed. Abuse of synonyms could make code confusing -for  instance, picture  a
long  program using  multiple names for common types like Int or String simulta-
neously.Incorporating the suggested type synonyms and the Date type we proposed:

> data Anniversary' = Birthday' Name Date
>                   | Wedding'  Name Name Date
>                  
> joeDoe :: Anniversary'
> joeDoe = Birthday' "Joe Doe" (Date 1968 3 4)
>
> doeWedding :: Anniversary'
> doeWedding = Wedding' "Joe Doe" "Jane Doe" (Date 1987 4 3)
>
> anniversariesOfJoeDoe :: AnniversaryBook
> anniversariesOfJoeDoe = [joeDoe, doeWedding]
>
> showDate'' :: Date -> String
> showDate'' (Date y m d) = show y ++ "-" ++ show m ++ "-" ++ show d
>
> showAnniversary'' :: Anniversary' -> String
> showAnniversary'' (Birthday' name date)       =
>         name ++ " born " ++ showDate'' date
> showAnniversary'' (Wedding' name1 name2 date) =
>         name1 ++ " married " ++ name2 ++ " on " ++ showDate'' date

And now you can evaluate in the Prelude:

Prelude> map showAnniversary'' anniversariesOfJoeDoe
["Joe Doe born 1968-3-4","Joe Doe married Jane Doe on 1987-4-3"]


