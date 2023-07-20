#lang scribble/rhombus/manual
@(import:
    rhombus/runtime_path
    rhombus/meta open
    meta_label:
      shplait open
    "eval.rhm".eval
    lib("scribble/core.rkt") as s_core
    lib("scribble/html-properties.rkt") as html_prop
    "tutorial_url.rhm" open
    lib("scribble/manual.rkt").racketresultfont)

@(def defterm = italic)

@(def ref_scrbl = ModulePath 'lib("scribblings/reference/reference.scrbl")')
@(def shrubbery_scrbl = ModulePath.s_exp(ModulePath 'lib("shrubbery/scribblings/shrubbery.scrbl")'))

@(fun pink(e):
    elem(~style: s_core.style(#false, [s_core.#{background-color-property}("pink")], e)))

@(runtime_path.def demo_rhm: "demo.rhm")
@(def demo_link:
    @elem(~style: s_core.style(#false, [html_prop.#{link-resource}(demo_rhm)])){@filepath{demo.rhm}})

@(macro 'interaction($term, ...)': 'examples(~eval: eval, $term, ...)')

@title(~style: #'toc, ~tag: "Tutorial"){Tutorial}

The
@hyperlink(tutorial_video_url){Shplait tutorial videos}
provide most of the same information as this section.

@margin_note{For a quick refresher of the tutorial content, try @(demo_link).}

@local_table_of_contents()

@// ----------------------------------------
@section(~tag: "getting-started"){Getting Started}

To get started with Shplait,
@hyperlink("https://download.racket-lang.org/"){download Racket}, install
it, start @onscreen{DrRacket}, and install the @tt{shplait} package using DrRacket's
@onscreen{Install Package...} menu item in the @onscreen{File} menu.

Then, in the top part of the DrRacket window, type

@rhombusblock(
  #,(hash_lang()) shplait
)

and click the @onscreen{Run} button (or use the keyboard shortcut
shown in the @onscreen{Racket} menu). In the bottom part of the window, type
@rhombus(1) and hit Return, and you should see this result:

@interaction(1)

In other words, the expression @rhombus(1) has type @rhombus(Number),
and the value of the expression is @rhombus(1, ~result).

In this tutorial, we will mostly show expressions as if typed in that
bottom area. You can also put the expressions in the top area and hit
@onscreen{Run} again, but in that case, the type of the result will
not print before the result.

In a few places, the tutorial shows lines that include double slashes,
@litchar{//}. A @litchar{//} starts a comment that continues to the end of
the line. For examples of other comment forms, see @(demo_link).

@// ----------------------------------------
@section(~tag: "data-tutorial"){Simple Data}

Shplait supports various kinds of numbers, all with type @rhombus(Number):

@interaction(
  1
  0.5
  1/3
)

The result of @rhombus(1/3) prints oddly because it's an exact number,
not an inexact floating-point number like the result of @rhombus(1.0)
divided by @rhombus(3.0). The full syntax of numbers is probably not
important, but it's
@seclink(~doc: shrubbery_scrbl, "top"){Shrubbery number syntax}.

The booleans @defterm{true} and @defterm{false} are written
@rhombus(#true) and @rhombus(#false):

@interaction(
  #true
  #false
)

Strings are written the usual way with a starting and ending @litchar{"}:

@interaction(
  "apple"
  "banana cream pie"
  "yes, \"escape\" quotes with backslash"
)

In addition to strings, Shplait includes string-like values called
@defterm{symbols}. A symbol is written with a hash and single quote
@litchar{#'} followed by a sequence of characters that are allowed in
identifiers (which does not include whitespace or operator characters).

@interaction(
  #'apple
  #'banana
  #'a_to_b
)

Characters like @litchar{-}, @litchar{>}, and @litchar{?} are not only
allowed in operatr names.

Individual characters are infrequently used in Shlait, and they do not
have a convenient syntax, but they can be written with @litchar{#\}
inside @litchar{#{}}:

@interaction(
  #{#\a}
  #{#\b}
  #{#\A}
  #{#\space} // same as #\ followed by a space
)

@// ----------------------------------------
@section(~tag: "built-ins-tutorial"){Using Built-in Operators and Functions}

Shplait includes some of the usual operators and functions on numbers, like
@rhombus(+) and @rhombus(max), used in the usual way.

@interaction(
  1 + 2
  floor(1.2)
  max(3, 5)
  ~error:
    1/0
)

Extra parentheses are allowed around an expression.

@interaction(
  (((1) + ((2))))
)

The type of a function is written with @rhombus(->). The function's
argument types appear before the arrow, and the function's result type
is after the arrow. If a function only has one argument, the argument
type can be written by itself, otherwise the argument types are
comma-separated and grouped with parentheses. A function is a value, so
if you evaluate just @rhombus(floor) or @rhombus(max) without calling
it, then Shplait will show the type and print that the result is a
function:

@interaction(
  floor
  max
)

Line breaks and indentation matter in Shplait. If you put @rhombus(1)
and @rhombus(+ 2) on different lines, they do not count as a use of the
@rhombus(+) operator.

@interaction(
  ~error:
    :
      1
      + 2
)

On occassion, it's useful to split an expression using an infix operator
across lines, and that's allowed as long at the operator appears at the
start of the second line, and as long at the second line is more
indented than the first.

@interaction(
  10000
    + 20000
)

Continue over even more lines by indenting to match the second line.

@interaction(
  10000
    + 20000
    + 30000
)

Function-call arguments can be on their own lines, but any new lines for
arguments must align. The first argument doesn't have to be more nested
than the function being calle,d but normally it is written that way.

@interaction(
  max(1,
      2)
  max(
    1,
    2
  )
)

Here are some example uses of other built-in operators and functions,
and you can click on any of the function names her eto jump to the
documentation:

@interaction(
  ! #true
  ! #false
  1 + 2
  1 - 2
  1 * 2
  1 < 2
  1 > 2
  1 == 2
  1 <= 2
  1 >= 2
  string_append("a", "b")
  "a" == "b"
  string_get("a", 0)
)

Note that some operations work on multiple types. For example,
@rhombus(==) works on any two values, as long as the two values
have the same type. That flexibility and constraint is reflected in
the type of @rhombus(==) by a symbol placeholder @rhombus(?a), which
you can read as ``a type to be picked later.'' A specific type is
picked for every individual use of @rhombus(==):

@interaction(
  1 == 1
  #'a == #'b
  ~error:
    1 == "a"
)

@// ----------------------------------------
@section(~tag: "cond-tutorial"){Conditionals}

The @rhombus(if) form works in the usual way, but with a syntax that
builds naturally to more complex conditionals. The ``test'' expression
for @rhombus(if) is followed by a vertical bar @litchar{|} before the
``then'' expression, and then another vertical bar @litchar{|} before
the ``else'' expression. The ``else'' expression cannot be omitted.

@interaction(
  if "apple" == "banana" | #'yes | #'no
)

A @litchar{|} can start on its own line. If it's the first @litchar{|}
for a form like @rhombus(if), then it needs to be aligned with the
@rhombus(if). A subsequence @litchar{|} needs to be aligned with the
first @litchar{|}.

@interaction(
  if "apple" == "banana"
  | #'yes
  | #'no
  if "apple" == "banana" | #'yes
                         | #'no
)

DrRacket can help you indent a suitable amount. Hit the Tab key to cycle
through syntactically vaid indentations for the current line. Note that
if your line doesn't yet start with @litchar{|}, then DrRacket will
offer different choices than when the line does start with @litchar{|}.

The content after a @litchar{|} can also start on its own line, as long
as it's more indented than the @litchar{|}. That choice is rarely made,
since the required indentation means that no horizontal space is saved.

@interaction(
  if "apple" == "banana"
  |
    #'yes
  |
    #'no
)

The @rhombus(cond) form is a multi-way @rhombus(if). A @rhombus(cond)
form has a sequence of clauses, where each clause has a ``question'' and
a result expression, and the two parts are seperated with a colon
@litchar{:}. The result expression is used only when the question
produces true. The @rhombus(cond) form tries the clauses in order, and
as soon as it finds a true result from a question, it produces the
corresponding result. The last clause's question cal be @rhombus(~else)
as a synonym for @rhombus(#true).

@interaction(
  cond
  | 2 < 1: 17
  | 2 > 1: 18
  cond
  | 2 < 1: 1/0 // result expression skipped
  | 2 > 1: 18
  cond
  | #true: 8
  | #true: 1/0 // second clause not reached
  cond
  | 3 < 1: 0
  | 3 < 2: 1
  | 3 < 3: 2
  | 3 < 4: 3
  cond
  | "a" == "b": 0
  | "a" == "c": 1
  | ~else: 2
)

The rules about @litchar{|} are the same for @rhombus(if),
@rhombus(cond), or any form, so a @rhombus(cond) form could be written
on a single line, although this is not encouraged.

@interaction(
  cond | "a" == "b": 0 | "a" == "c": 1 | ~else: 2
)

Similarly, a general rule applies about @litchar{:}. The content after a
@litchar{:} can start on its own line, as long as it's more indented
that the content before @litchar{:}. The indentation doesn't need to be
more than the @litchar{:} itself---only more than the content before a
@litchar{:}---so starting on a new line can save a lot of horizontal
space, and it's a good and common choice.

@interaction(
  cond
  | "a" == "b":
      0
  | "a" == "c":
      1
  | ~else:
      2
)

``And'' and ``or'' operations are written with @rhombus(&&) and
@rhombus(||) infix operators. The @rhombus(&&) and @rhombus(||) forms
are short-cicuiting, too, and they can be chained to work with any
number of boolean subexpressions:

@interaction(
  #true && #true
  #true && #false
  2 < 1 && #true
  2 < 1 && 1/0 == 0 // second expression is not evaluated}
  #false || #true
  #false || #false
  1 < 2 || 1/0 == 0 // second expression is not evaluated
  #true && #true && #true
  #false || #false || #false
)

@// ----------------------------------------
@section(~tag: "lists-tutorial"){Lists}

Shplait lists are @defterm{uniform}, meaning that all of the elements of a
list must have the same type. Square brackets @litchar{[]} create a list
with comma-separated elements.

@interaction(
  [1, 2, 3 + 4]
  [string_append("a", "b"), "c"]
)

The newline and indentation rules for @litchar{[]} are the same as for
function-call parentheses.

@interaction(
  [1,
   2,
   3 + 4]
  [1, 2, 3, 4,
   5, 6, 7, 8,
   9, 10, 11]
  [
    1,
    2,
    3
  ]
)

As you can see, the type of a list is written with @rhombus(Listof) and
then the type of the elements of the list in parentheses.

A list is immutable. That is, the value @rhombus([1, 2, 3]) is as
unchanging as the numbers @rhombus(1), @rhombus(2), and @rhombus(3)
within the list. You can't change a list to add new elements to
it---but you can create a new list that is like the old one, except
that it has another element. The @rhombus(cons) function takes an
element and a list and ``adds'' the element to the front of the list,
creating a new list with all of the elements:

@interaction(
  cons(1, [2, 3])
  cons("apple", ["banana"])
)

The @rhombus(cons) operation is constant-time, because a list is
internally represented as a singly linked list, and @rhombus(cons)
simply creates a new cell that contains the new value and then points
to the existing list.

If you have two lists, instead of one element and a list, you can
combine the lists with @rhombus(append):

@interaction(
  append([1, 2], [3, 4])
)

Don't confuse @rhombus(cons) and @rhombus(append). The @rhombus(cons)
function takes an @defterm{element} and a list, while @rhombus(append)
takes a @defterm{list} and a list. That difference is reflected in
their types:

@interaction(
  cons
  append
)

Mixing them up will trigger a type error:

@interaction(
  ~error:
    cons([1], [2, 3])
  ~error:
    append(1, [2, 3])
)

A list doesn't have to contain any values:

@interaction(
  []
)

Although the multi-element @litchar{[]} list form may seem fundamental,
the true list-construction primitives are @rhombus([]) and
@rhombus(cons), and you can build up any other list using those:

@interaction(
  []
  cons("food", [])
  cons("dog", cons("food", []))
)

The @rhombus(is_empty) function determines whether a list is empty, and
@rhombus(is_cons) determines whether a list has at least one item:

@interaction(
  is_empty([])
  is_cons(cons(1, []))
  is_cons([1])
  is_cons([])
  is_empty([1])
)

The @rhombus(cons) operation constructs a new value from two pieces.
The @rhombus(first) and @rhombus(rest) operations are the opposite of
@rhombus(cons). Given a value produced by @rhombus(cons), @rhombus(first)
returns the item that @rhombus(cons) added to the start of the list,
and @rhombus(rest) returns the list that @rhombus(cons) added to. More
generally, @rhombus(first) gets the first item from a list, and
@rhombus(rest) gets everything list in the list when the first argument
is removed.

@interaction(
  first(cons(1, [2, 3]))
  rest(cons(1, [2, 3]))
  first(["apple", "banana", "coconut"])
  rest(["apple", "banana", "coconut"])
  first(rest(["apple", "banana", "coconut"]))
  rest(rest(["apple", "banana", "coconut"]))
)

Shplait also provides @rhombus(list_get) for getting an element by its
index, which is sometimes useful to extract pieces of a list that has a
known shape. Functions that take the @rhombus(first) of a list and recur
with the @rhombus(rest) turn out to be be more common. Here's a function
that check whether @rhombus("milk") is in a list of strings (and we
explainmore about definitions in the next section):

@interaction(
  ~defn:
    fun got_milk(items :: Listof(String)) :: Boolean:
      cond
      | is_empty(items): #false
      | is_cons(items):
          (first(items) == "milk") || got_milk(rest(items))
  ~repl:
    got_milk([])
    got_milk(["milk", "cookies"])
    got_milk(["cookies", "milk"])
    got_milk(["cookies", "cream"])
)

@// ----------------------------------------
@section(~tag: "definitions-tutorial"){Definitions}

The @rhombus(def) form defines an identifier to be a synonym for a
value:

@interaction(
 def pi = 3.14
 pi
 def tau = pi + pi
 tau
)

You can use either @litchar{=} or @litchar{:} to separate the defined
name from its value. Use @litchar{=} when everything is on one line,
which is the usual case, and use @litchar{:} (with the usual rule for
@litchar{:} indentation) when multiple lines are needed.

The @rhombus(fun) form defines a function. The function name is followed
by an open parenthesis, a comma-separated sequence of argument names,
and a closing parenthesis. After that, use a @litchar{:} then the body
of the function, which can refer to the function arguments and is
evaluated when the function is called.

@interaction(
  fun circle_area(r):
    pi * r * r
  circle_area(10)
)

Since @secref("getting-started"), we have been evaluating forms only
in DrRacket's bottom area, which is also known as the
@defterm{interactions area}. Definitions normally go in the top
area---which is known as the @defterm{definitions area}, naturally.

Put these two definitions in the definitions area:

@interaction(
  ~hidden:
    fun num_is_even(x :: Number): #true
)

@interaction(
  ~defn:
    fun num_is_odd(x):
      if x == 0
      | #false
      | num_is_even(x-1)
  ~defn:
    fun num_is_even(x):
      if x == 0
      | #true
      | num_is_odd(x-1)
)

Click @onscreen{Run}. The functions @rhombus(num_is_odd) and @rhombus(num_is_even)
are now available in the interactions area:

@interaction(
  is_odd
  is_odd(12)
)

In our definitions of @rhombus(pi) and @rhombus(tau), Shplait inferred
that the newly defined names have type @rhombus(Number) and that
@rhombus(num_is_odd) has type @rhombus(Number -> Boolean). Programs are
often easier to read and understand if you write explicitly the type
that would otherwise be inferred. Declaring types can sometimes help
improve or localize error messages when Shplait's attempt to infer a
type fails, since inference can other end up depending on the whole
program.

Declare a type for a constant by writing @rhombus(::) followed by a type
after the defined identifier:

@interaction(
  ~defn:
    def groceries :: Listof(String) = ["milk", "cookies"]
)

Alternatively, you can declare an idenitifier's type separate from its
definition by using @rhombus(::).

For a function, attach a type to an argument using @rhombus(::) and a
type after the argument name. Write the function's result type with
@rhombus(::) and the type after the parentheses that group the arguments
and before the @litchar{:} for the function body.

@interaction(
  ~defn:
    fun starts_milk(items :: Listof(String)) :: Boolean:
      is_cons(items) && first(items) == "milk"
)

You can declare local functions and constants by using the
@rhombus(block) form. A @litchar{:} appears immediately after
@rhombus(block), and it is followed byits body as any number of
definitions and expressions, each on its own line. The last form in a
@rhombus(block) body ust be an expression, and that expression supplies
the result for the whole @rhombus(block) form.

@interaction(
  block:
    def pi_ish = 3
    fun approx_circle_area(r):
      pi_ish * r * r
    approx_circle_area(2)
  ~error:
    pi_ish // not visible outside the block
  ~error:
    approx_cirle_area // not visible outside the block
)

The @rhombus(block) form is most often used inside a function to define
a helper function or to avoid a repeated computation involving the
function arguments.

@interaction(
  ~defn:
    fun discard_first_if_fruit(items):
      block:
        def a = first(items)
        cond
        | a == "apple": rest(items)
        | a == "banana": rest(items)
        | ~else: items
  ~repl:
    discard_first_if_fruit(["apple", "potato"])
    discard_first_if_fruit(["banana", "potato"])
    discard_first_if_fruit(["potato", "apple"])
)

The @rhombus(let) and @rhombus(letrec) forms are similar to
@rhombus(block) with on definition, but they are somewhat more compact
by avoiding the requirement to write both @rhombus(block) and
@rhombus(def). The @rhombus(discard_first_if_fruit) example above can be
equivalently written using @rhombus(let):

@interaction(
  ~defn:
    fun discard_first_if_fruit(items):
      let a = first(items):
        cond
        | a == "apple": rest(items)
        | a == "banana": rest(items)
        | ~else: items
)

@// ----------------------------------------
@section(~tag: "datatypes-tutorial"){Datatypes}

So far, we have only seen built-in types like @rhombus(Number) and
@rhombus(Listof(String)). Sometimes, it's useful to define your own
name as a shorthand for a type, such as defining @rhombus(Groceries) to
be equivalent to @rhombus(Listof(String)):

@interaction(
  ~defn:
    type Groceries = Listof(String)
    def shopping_list :: Groceries = ["milk", "cookies"]
)

Note that, by convention, all type names are capitalized. Shplait is
case-sensitive.

But what if the data that you need to represent is not easily encoded
in existing types, such as when you need to keep track of a tiger's
color and stripe count (which doesn't work as a list, since a list
can't have a number and a string)? And what if your type, say
@rhombus(Animal), has values of different shapes: tigers that have color
and stripe counts, plus snakes that have a color, weight, and favorite
food?

The @rhombus(type) form handles those generalizations when you write
cases with @litchar{|} instead of using @rhombus(=). The general form is

@rhombusblock(
  type #,(@rhombus(Type, ~var))
  | #,(@rhombus(variant_name_1, ~var))(#,(@rhombus(field_name_1, ~var)) :: #,(@rhombus(Type_1, ~var)),
                                       #,(@rhombus(field_name_2, ~var)) :: #,(@rhombus(Type_2, ~var)),
                                       ...)
  | #,(@rhombus(variant_name_2, ~var))(#,(@rhombus(field_name_3, ~var)) :: #,(@rhombus(Type_3, ~var)),
                                       #,(@rhombus(field_name_4, ~var)) :: #,(@rhombus(Type_4, ~var)),
                                       ...)
  | ...
)

with any number of @defterm{variants} and where each variant has any
number of typed @defterm{fields}. If you're used to Java-style
classes, you can think of @rhombus(Type, ~var) as an interface, and each
variant is a class that implements the interface. Unlike Java classes,
a variant name doesn't work as a type name; it only works to create
an instance of the variant.

For example, the following definition is suitable for representing
animals that can be either tigers or snakes:

@interaction(
  ~defn:
    type Animal
    | tiger(color :: Symbol,
            stripe_count :: Number)
    | snake(color :: Symbol,
            weight :: Number,
            food :: String)
)

After this definition, @rhombus(Animal) can be used as a type, while
@rhombus(tiger) and @rhombus(snake) work as functions to create
@rhombus(Animal)s:

@interaction(
  tiger(#'orange, 12)
  snake(#'green, 10, "rats")
)

The definition of @rhombus(Animal) also cooperates with @rhombus(is_a)
and creates several additional functions:

@itemlist(

 @item{@rhombus(#,@(rhombus(expr, ~var)) is_a tiger), for an
  @rhombus(expr, ~var) that produces an @rhombus(Animal) determines
  whether the @rhombus(Animal) value was created by @rhombus(tiger) (as
  opposed to @rhombus(snake));}

 @item{@rhombus(#,@(rhombus(expr, ~var)) is_a snake), similarly
  determines whether the @rhombus(Animal) value of @rhombus(expr, ~var)
  was created by @rhombus(snake);}

 @item{@rhombus(tiger.color) and @rhombus(tiger.stripe_count) are
  functions that take a tiger @rhombus(Animal) and extract its color and
  stripe count, respectively; and}

 @item{@rhombus(snake.color), @rhombus(snake.weight), and
  @rhombus(snake.food) are functions that take a snake @rhombus(Animal)
  and extract its color, weight, and favorite food, respectively.}

)

@interaction(
  def tony = tiger(#'orange, 12)
  def slimey = snake(#'green, 10, "rats")
  tony is_a tiger
  slimey is_a tiger
  tiger.color(tony)
  snake.food(slimey)
  ~error:
    tiger.color(slimey)
)

Note that the type of @rhombus(tiger.color(slimey)) printed before an
error was reported. That's because @rhombus(tiger.color(slimey)) is
well-typed as far as Shplait can tell, since @rhombus(tiger-color) wants
an @rhombus(Animal) and @rhombus(slimey) has type @rhombus(Animal).
We'll see that @rhombus(match) provides an alterntive to selectors like
@rhombus(tiger.color) that is less dangerous than the selector.

Using @rhombus(Animal) as a type and the @rhombus(is_a tiger) and
@rhombus(is_a snake) predicates, we can write a function that extracts
the color of any animal:

@interaction(
  ~defn:
    fun animal_color(a :: Animal) :: Symbol:
      cond
      | a is_a tiger: tiger.color(a)
      | a is_a snake: snake.color(a)
  ~repl:
    animal_color(tony)
    animal_color(slimey)
)

When writing @rhombus(animal_color), what if we forget the
@rhombus(is_a snake) case? What if we get @rhombus(snake.color) and
@rhombus(tiger.color) backwards? Unfortunately, the type checker cannot
help us detect those problems. If we use @rhombus(match), however, the
type checker can help more.

The general form of a @rhombus(match) expresison is

@rhombusblock(
  match #,(@rhombus(val_expr, ~var)):
  | #,(@rhombus(variant_name_1, ~var))(#,(@rhombus(field_name_1, ~var)), #,(@rhombus(field_name_2, ~var)), ...):
      #,(@rhombus(result_expr_1, ~var))
  | #,(@rhombus(variant_name_2, ~var))(#,(@rhombus(field_name_3, ~var)), #,(@rhombus(field_name_4, ~var)), ...):
      #,(@rhombus(result_expr_2, ~var))
  | ...
)

The @rhombus(val_expr, ~var) must produce a value matching the same type
as the @rhombus(variant_name, ~var)s, which must all be from the same
type. Every variant of the type must be represented by a clause with a
matching @rhombus(variant_name, ~var), and for that clause, the number
of @rhombus(field_name, ~var)s must match the declared number of fields
for the variant. The type checker can check all of those requirements.

To produce a value, @rhombus(match) determines the variant that is
instanited by the result of @rhombus(val_expr, ~var). For the clause
matching that variant (by name), @rhombus(match) makes each
@rhombus(field_name, ~var) stand for the corresponding field (by position)
within the value, and then evaluates the corresponding
@rhombus(result_expr, ~var).

Here's @rhombus(animal_color) rewritten with @rhombus(match):

@interaction(
  ~defn:
    fun animal_color(a :: Animal) :: Symbol:
      match a
      | tiger(col, sc): col
      | snake(col, wgt, f): col
  ~repl:
    animal_color(tony)
    animal_color(slimey)
)

Put the definitions of @rhombus(Anmal) and @rhombus(animal_color) in
DrRacket's definitions area. Then, you can mouse over @rhombus(a) in
@rhombus(animal_color) to confirm that it means the @rhombus(a) that is
passed as an argument. Mouse over @rhombus(col) to see that it means
one of the variant-specific fields. Try changing the body of
@rhombus(animal_color) to leave out a clause or a field variable and
see what error is reported when you hit @onscreen{Run}.

You should think of @rhombus(match) as a pattern-matching form. It
matches a value like @rhombus(tiger(#'orange, 12)) to the pattern
@rhombus(tiger(col, sc)) so that @rhombus(col) stands for
@rhombus(#'orange) and @rhombus(sc) stands for @rhombus(12). A value like
@rhombus(snake(#'green, 10, "rats")) does not match the pattern
@rhombus(tiger(col, sc)), but it matches the pattern
@rhombus(snake(col, wgt, f)).

At the end of @secref("lists-tutorial"), we saw a @rhombus(got_milk)
function that uses @rhombus(cond), similar to the way the dangerous
version of @rhombus(animal_color) uses @rhombus(cond). The
@rhombus(match) form works on list types with @rhombus([]) and
@rhombus(cons(fst, rst)) patterns, so here's an improved
@rhombus(got_milk):

@interaction(
  ~defn:
    fun got_milk(items :: Listof(String)):
      match items
      | []: #false
      | cons(item, rst_items): item == "milk" || got_milk(rst_items)
  ~repl:                   
    got_milk([])
    got_milk(["cookies", "milk"])
)

The @rhombus([]) pattern is a special case in @rhombus(match), and can
only be used with @rhombus(cons) or @rhombus(~else) in the other case.
Otherwise, a pattern witll have a variant name followed by arguments in
parentheses. Even if a variant has no fields, it will have an empty pair
of parentheses in its construction and pattern.

@interaction(
  ~defn:
    type Grade
    | letter(alpha :: String)
    | pass_fail(is_pass :: Boolean)
    | incomplete()
  ~repl:
    letter("A")
    pass_fail(#true)
    incomplete()
  ~defn:
    fun passed_course(g :: Grade) :: Boolean:
      match g
      | letter(a): a != "F"
      | pass_fail(is_p): is_p
      | incomplete(): #false
  ~repl:
    passed_course(letter("B"))
    passed_course(incomplete())
)

You can also use @rhombus(~else) for a final clause in @rhombus(match)
to catch any variants that are not already covered.

@interaction(
  ~defn:
    fun is_high_pass(g :: Grade) :: Boolean:
      match g
      | letter(a): a == "A"
      | ~else: #false
  ~repl:
    is_high_pass(letter("A"))
    is_high_pass(incomplete())
)

When you use @rhombus(~else), however, the type checker is less helpful
for making sure that you've considered all cases.

@// ----------------------------------------
@section(~tag: "testing-tutorial"){Testing and Debugging}

Shplait includes built-in support for testing your programs. The
@rhombus(check) form takes two expressions and makes sure that they
produce the same value, where the second expression is prefixed with
@rhombus(~is). Typically, the first expression is a function call, and
the second expression is the expected result of the call. The
@rhombus(check) form is silent if a test passes, but it prints an error
message (and continues) if the test fails.

@interaction(
  ~defn:
    fun taste(s):
      cond
      | s == "milk": #'good
      | ~else: #'not_as_good
  ~defn:
    check:
      taste("milk")
      ~is #'good
    check:
      taste("brussel sprouts")
      ~is #'not_as_good
    check:
      taste("beets")
      ~is #'bad

)

To test that an expression reports an expected error, use
@rhombus(~raises) in place of @rhombus(~is), and then supply
an expression that produces a string after @rhombus(~raises).
The test checks that the exception's error message contains the
string.

@interaction(
  ~defn:
    fun always_fail(n :: Number) :: Number:
      error(#'always_fail, "we're not actually returning a number")
  ~defn:
    check:
      always_fail(42)
      ~raises "not actually"
    check:
      always_fail(42)
      ~raises "should not get called"
)

When you write a program (in the definitions area of DrRacket), the
order of function definitions generally does not matter, even if the
functions call each other. A test at the top level of a program,
however, must appear after all functions that the test may end up
calling. To relax this constraint, wrap tests in a
@rhombus(module test: ....) form. A @rhombus(module test: ....))
wrapper effectively moves its content to the end of the program.

@rhombusblock(
  module test:
    check:
      retaste("milk")
      ~is [#'still, #'good]

  fun retaste(s):
    [#'still, taste(s)]
)

A good set of tests will cause all expressions in a program to be
evaluated at least once. DrRacket can help you check that your program
has good test coverage. In DrRacket's @onscreen{Language} menu, select
@onscreen{Choose Language}, click @onscreen{Show Details}, and then
select the @onscreen{Syntactic test suite coverage} option. After
selecting that option, when you @onscreen{Run} a program, it will stay
its normal color if all is well. If some expression has not been
covered, however, the program text will go mostly black, and any
expression that has not been evaluated will turn orange with a black
background. Resolve the problem and restore your program text's color by
adding more tests.

@(block:
    #void

/*

When you're debugging a program, it may be helpful to see the
arguments that are passed to a particular function and the results
that the function returns. You can enable that kind of tracing for a
function with the @rhombus(trace) declaration, which must appear after
the function's definitions.

@racketblock[
(define (got-milk? [_items : (Listof String)])
  (type-case (Listof String) _items
    [empty #false]
    [(cons _item _rst-items) (or (string=? _item "milk")
                                 (got-milk? _rst-items))]))
(trace got-milk?)
]

@interaction[
#:hidden
(require (typed-in racket/trace [trace : ('a -> Void)] [untrace : ('a -> Void)]))
(define supressed (trace got-milk?))
]

@interaction[
(got-milk? empty)
(got-milk? '("cookies" "milk"))
]

@interaction[#:hidden (untrace got-milk?)]

As you're devloping a program, sometimes it's useful to run a partial
program where you haven't yet decided on part of the implementation.
The @rhombus(....) expression (with four dots) can be used in place of
any expression of any type. A program using @rhombus(....) can compile
and run, but the @rhombus(....) reports an error if it is reached
during evaluation.

@interaction[
(define (got-milk? [_items : (Listof String)])
  (type-case (Listof String) _items
    [empty #false]
    [(cons _item _rst-items) ....]))
(got-milk? '())
(eval:error (got-milk? '("cheese")))
]

*/
   )

@// ----------------------------------------
@section(~tag: "lambda-tutorial"){Anonymous Functions}

After we define a function, the name of the function can be used as a
value without calling it. If you just evaluate the function name, then
Shplait will print something like @nonbreaking{@racketresultfont{#<function>}}.

@interaction(
  ~defn:
    fun plus_five(n):
      n + 5
  ~repl:
    plus_five
)

More usefully, you might pass the function to another function that
calls it. For example, the @rhombus(map) function takes a function and a
list, and it applies the function to each element of the list to
produce a new list.

@interaction(
  map(plus_five,
      [1, 2, 3])
)

Sometimes, and especially with @rhombus(map), you need a one-off
function that doesn't need to be defined for everyone else to see, and
it doesn't even need a name. You can make an @defterm{anonymous function}
by using @rhombus(fun) without a name before the arguments:

@interaction(
  map(fun (n):
        n + 6,
      [1, 2, 3])  
)

The form @rhombus(fun (n): n + 6) means ``the function that
takes an argument @rhombus(n) and returns @rhombus(n + 6).'' You can
evaluate a @rhombus(fun) form without passing it anywhere, although
that isn't particularly useful:

@interaction(
  fun (n):
    n + 7
)

Notice that the result has a function type: it's a function that takes
a @rhombus(Number) and returns a @rhombus(Number).

An anonymous function created with @rhombus(fun) doesn't have to
@emph{stay} anonymous. Since you can use a @rhombus(fun) form anywhere
that an expression is allowed, you can use in @rhombus(def):

@interaction(
  ~defn:
    def plus_eight = (fun (n):
                        n + 8)
)

This definition is completely equivalent to the function-definition
shorthand:

@interaction(
  ~defn:
    fun plus_eight(n):
      n + 8
)

Another interesting property of anonymous functions is that,
just like any local function, the body of a @rhombus(fun) can see any
surrounding variable binding. For example, the @rhombus(fun) body in
the following @rhombus(add_to_each) function can see the @rhombus(m)
that is passed to @rhombus(add_to_each):

@interaction(
  ~defn:
    fun add_to_each(m, items):
      map(fun (n):
            n + m,
          items)
  ~repl:
    add_to_each(7, [1, 2, 3])
    add_to_each(70, [1, 2, 3])
)

You can declare types for @rhombus(fun) arguments and results similar to
declaring them with the function-definition shorthand:

@interaction(
  ~defn:
    fun (s :: String) :: Boolean:
      string_length(s) > 10
)

@// ----------------------------------------
@section(~tag: "syntax-object"){Syntax Objects}

If we write @rhombus(pi + pi), then given our
@seclink("definitions-tutorial"){earlier definition} of @rhombus(pi),
the result is as you'd expect, even if we include a lot of extra space
around the @rhombus(+):

@interaction(
  pi    +  pi
)

We could add string quotes around the expression and get a
completely different result:

@interaction(
  "pi    +  pi"
)

If you're studying programming languages and building interpreters, then
a string representation of program text is potentially useful. Still,
parsing the @rhombus(pi) identifiers and @rhombus(+) operator out of
that string, including ignoring the unimportant whitespace, is a lot of
work.

Shplait offers a different kind of quoting via single-quote marks:

@interaction(
  'pi    +  pi'
)

Supreficially, this interaction is similar to the one with a string, but
notice two things: the type was reported as @rhombus(Syntax), and the
whitespace has been normalized. With a string, only individual
characters can be extracted, but with a @tech{syntax object}, structured
syntax components can be extracted. For example, @rhombus(syntax_split)
extracts the terms of a single-group (roughly: single-line) syntax
object.

@interaction(
  syntax_split('pi    +  pi')
)

Parenthesized terms and blocks formed with @litchar{:} count as
individual terms for @rhombus(syntax_split), although they also have
nested structure.

@interaction(
  syntax_split('1 + (3 + 4)')
  syntax_split('block: 1')
)

@(block:
    #void

/*

If you've had some experience programming in Java, you might think
that the solution is a list of @tt{Object}s, because anything can be
coerced to and from the type @tt{Object}. That is, we would be able to
mix a symbol as @tt{Object} with two numbers as @tt{Object}s.

Shplait doesn't have an @tt{Object} type, but it does have an
@rhombus(S-Exp) type, which is similar. Even better, the @rhombus(S-Exp)
type works with a convenient @litchar{'}-like shortcut. The
@rhombus(S-Exp) shortcut is @litchar{`} (usually pronounced
``backquote'') instead of @litchar{'}:

@interaction(
  '1 + 2'
)

When an S-expression is list-like, then you can corece the
S-expression to a list using @rhombus(s-exp->list). The result is a
list of @rhombus(S-Exp)s:

@interaction(
  syntax_to_list('[1, 2, 3]')
]

If an S-expression isn't list-like, the coercion fails. Other
coercions include @rhombus(s-exp->number) and @rhombus(s-exp->symbol).
You can go the other way with @rhombus(number->s-exp),
@rhombus(symbol->s-exp), and @rhombus(list->s-exp). Functions like
@rhombus(s-exp-list?) and @rhombus(s-exp-number?) report whether an
S-expression is list-like or number-like.

@interaction[
`1
(eval:error (s-exp->list `1))
(s-exp->number `1)
(number->s-exp 1)
(list->s-exp (list (symbol->s-exp '+)
                   (number->s-exp 1)
                   (number->s-exp 2)))
(s-exp-number? `1)
(s-exp-list? `1)
]

The backquote @litchar{`} versus forward quote @litchar{'} distinction
is subtle. A convention to help highlight the difference is to mostly
use curly braces with @litchar{`}. Curly braces are interchangable
with parentheses and square brackets, and Shplait won't print results
with curly braces, but the visual cue can still help when reading
programs.

@interaction[
`{+ 1 2}
`{* 3 {+ 4 x}}
]

The S-expression @litchar{`} has an extra feature that the
list-constructing @litchar{'} lacks: a way to escape back to the
evaluated-expression world by using @litchar{,} (i.e., a comma). The
escaped expression must produce a S-expression, and the result
S-expression takes the place of the escape:

@interaction[
`{+ 1 ,(number->s-exp (+ 3 4))}
]

The @litchar[",@"] escape form is similar to @litchar{,}, but
@litchar[",@"] is a @defterm{splicing} escape that expects a list of
S-expressions and inlines the elements into the enclosing list-like
S-expression.

@interaction[
`{+ ,@(list (number->s-exp 1) (number->s-exp (+ 3 4)))}
`{+ ,(list->s-exp (list (number->s-exp 1) (number->s-exp (+ 3 4))))}
]

@; ----------------------------------------
@section(~tag: "s-exp-match-tutorial"){S-Expression Matching}

Since the @rhombus(equal?) function works on any kind of value, it can
compare two S-expressions to determine whether they are the same:

@interaction[
(equal? `{+ 1 2} `{+ 1 2})
(equal? `{+ 1 2} `{+ 1 4})
]

Suppose that you don't just want to recognize @rhombus(`{+ 1 2}), but
you want to recognize any list-like S-expression that has three
elements where the first element is @rhombus('+)-like and the other two
elements are number-like. That recognition problem is tedious to
implement, due to the many required many checks and coercions.

@interaction[
(define (is-plus-numbers? se)
  (and (s-exp-list? se)
       (let ([l (s-exp->list se)])
         (and (= 3 (length l))
              (let ([a (first l)])
                (and (s-exp-symbol? a)
                     (eq? '+ (s-exp->symbol a))))
              (s-exp-number? (second l))
              (s-exp-number? (third l))))))
(is-plus-numbers? `{+ 1 2})
(is-plus-numbers? `1)
(is-plus-numbers? `{+ 3 y})
(is-plus-numbers? `{{+} 1 2})
]

The @rhombus(s-exp-match?) function simplifies recognition tasks for
S-expressions. It's like @rhombus(equal?) on S-expressions, but the
first S-expression can have special symbols that match different
classes of values, instead of matching themselves literally. The
special symbols include @rhombus(NUMBER), which matchs any number, so
@rhombus(is-plus-numbers?) is more simply implemented like this:

@interaction[
(define (is-plus-numbers? se)
  (s-exp-match? `{+ NUMBER NUMBER} se))
(is-plus-numbers? `{+ 1 2})
(is-plus-numbers? `{+ 3 y})
]

Other special symbols include @rhombus(SYMBOL), which matches any
symbol, and @rhombus(ANY), which matches anything.

@interaction[
(define (single-argument-lambda? se)
  (s-exp-match? `{lambda {SYMBOL} ANY} se))
(single-argument-lambda? `{lambda {x} {+ x 1}})
(single-argument-lambda? `{lambada 0})
]

The symbol @rhombus(...) is even more special. It causes the preceeding
S-expression to match zero or more times to cover multiple elements in
an enclosing list. For example, @rhombus(`{SYMBOL ...}) would match a
list-like S-expression that has any number of symbol-like elements.

@interaction[
(define (any-argument-lambda? se)
  (s-exp-match? `{lambda {SYMBOL ...} ANY} se))
(any-argument-lambda? `{lambda {x} {+ x 1}})
(any-argument-lambda? `{lambda {x y z} {+ x 1}})
(any-argument-lambda? `{lambda {} {+ x 1}})
(any-argument-lambda? `{lambada 0})
]

@; ----------------------------------------
@section(~tag: "tuples-tutorial"){Tuples and Options}

If you want to combine a small number of values in a single value, and
if the values have different types (so that a list doesn't work), you
can use a @defterm{tuple} as an alternative to creating a new datatype
with a single variant.

The @rhombus(values) form creates a tuple from any number of values.
The type of a tuple reveals the type of every component value in the
tuple, separating the types with @rhombus(*).

@interaction[
(values 1 "milk" 'apple)
(values '(1 2 3) #false)
]

Using @rhombus(values), this @rhombus(consume) function can effectively
return two values each time that it is called:

@interaction[
#:no-prompt
(define (consume [s : String]) : (Symbol * String)
  (cond
   [(equal? s "milk") (values 'drink "Mmm....")]
   [(equal? s "beets") (values 'eat "Ugh....")]
   [else (values 'sleep "Zzz...")]))
]

To extract the component values from a tuple, match the tuple with
names using @rhombus(define-values).

@interaction[
(consume "milk")
(define-values (action response) (consume "beets"))
action
response
]

The convenience functions @rhombus(fst) and @rhombus(snd) can be used in
the special case of a 2-value tuple to extract the first or second
component.

@interaction[
(snd (consume "milk"))
]

Sometimes, instead of always returning multiple values, you'll want a
function that returns either one value or no value. A tuple is no help
for that case, but Shplait predefines a helpful datatype called
@rhombus(Optionof):

@racketblock[
(define-type (Optionof 'a)
  (none)
  (some [v : 'a]))
]

The @rhombus('a) in this definition of @rhombus(Optionof) indicates that
you can return any kind of value in a @rhombus(some).

@interaction[
(define (get-slogan [_s : String]) : (Optionof String)
  (cond
   [(equal? _s "milk") (some "It does a body good")]
   [else (none)]))
(get-slogan "milk")
(get-slogan "iced tea")
(type-case (Optionof String) (get-slogan "moon pie")
  [(some s) s]
  [(none) "no comment"])
]

@; ----------------------------------------
@section(~tag: "program-tutorial"){Programs and Modules}

When you write a program using @rhombus(@#,hash-lang[)
@#,racketmodname[plait]], you are technically defining a
@defterm{module}. A Shplait module contains a mixture of expressions and
definitions. The expressions are evaluated in order, and the value of
each expression is printed after the expression is evaluated (unless
the result value has type @rhombus(Void)). The order of function
definitions doesn't matter, as long as a function definition appears
before any expression that eventually calls the function.

@racketmod[
plait
code:blank
(define (is-odd? _x)
  (if (zero? _x)
      #false
      (is-even? (- _x 1))))
code:blank
(is-odd? 0) (code:comment "ok")
@#,tt{#;}(is-odd? 1) (code:comment @#,t{won't work, because it needs @rhombus(is-even?)})
code:blank
(define (is-even? _x)
  (if (zero? _x)
      #true
      (is-odd? (- _x 1))))
code:blank
(is-even? 1) (code:comment "ok")
(is-odd? 1) (code:comment "ok")
]

Note the use of @litchar{#;} in the example above. A @litchar{#;}
comments out the entire form that follows it, which is handy for
commenting out a definition of expression, even when the definition or
expression spans multiple lines.

Modules written with the @rhombus(module) form can be nested in other
modules. A nested module is called a @defterm{submodule}. Shplait
programs don't often use submodules that are written with
@rhombus(module), but the @rhombus(module+) form is more common. A
@rhombus(module+) form creates a submodule by merging all
@rhombus(module+)s that use the same name. A typical use of
@rhombus(module+) is to move all of a program's tests into a
@rhombus(test) submodule.

@racketmod[
plait
code:blank
(define (is-odd? _x)
  (if (zero? _x)
      #false
      (is-even? (- _x 1))))
code:blank
(module+ test
  (is-odd? 0)
  (is-odd? 1))
code:blank
(define (is-even? _x)
  (if (zero? _x)
      #true
      (is-odd? (- _x 1))))
code:blank
(module+ test
  (is-even? 1)
  (is-odd? 1))
]

The submodule name @rhombus(test) is special, because DrRacket
automatically runs a @rhombus(test) submodule (if one is present) after
running the enclosing module. In the above example, since the
@rhombus(test) submodule is run after the encloding module that defines
@rhombus(is-odd?) and @rhombus(is-even?), the tests can use all of the
functions. Another advantage of putting tests in a @rhombus(test)
submodule is that you can turn off the tests. In DrRacket's
@onscreen{Language} menu, select @onscreen{Choose Language}, click
@onscreen{Show Details}, click @onscreen{Submodules to run}, and then
uncheck the @onscreen{test} item.

A Shplait module's definitions are automatically exported from the
module. You can import the definitions of another module by using the
@rhombus(require) form, typically with a string that is a relative path
to the module to import.

@racketmod[#:file "math.rkt"
plait
(define pi 3.14)
(define tau (+ pi pi))
]

@racketmod[#:file "circle.rkt"
plait
(require "math.rkt")

(define (circle-area [r : Number]) : Number
  (* pi (* r r)))
]

A submodule created by @rhombus(module+) automatically imports the
bindings of the enclosing module, which is why @rhombus((module+ test
....)) submodules can automatically access definitions for testing. In
contrast, if you write definitions inside @rhombus((module+ test
....)), then the definitions can be used for tests in any
@rhombus((module+ test ....)), but the enclosing module will not see
the definitions.

@; ----------------------------------------
@section(~tag: "state-tutorial"){State}

@nested[#:style 'inset]{ @bold{Warning:} If you are using Shplait with a
programming-languages course, then the instructor has almost certainly
disallowed the constructs in this chaper for use in your homework
solutions, except as specifically allowed. Don't use @rhombus(set!),
@rhombus(begin), boxes, or vectors unless the instructor says that you
can. If you're tempted to use one of those, you're doing it wrong.}

We have so far described @rhombus(define) as naming constants, but
names bound by @rhombus(define) are not necessarily constant. The value
associated to the name can be changed using @rhombus(set!).

@interaction[
(define gravity 6.6e-11)
gravity
(set! gravity 6.5e-11)
gravity
]

The type of a @rhombus(set!) expression is @rhombus(Void), meaning that
it doesn't return a useful value, and the useless value doesn't even
print as a result. If you need to change a variable and then return a
value, use @rhombus(begin) to sequence the operations. The value of a
@rhombus(begin) form is the value of its last expression.

@interaction[
(define counter 0)
(define (fresh-number!)
  (begin
    (set! counter (add1 counter))
    counter))
(fresh-number!)
(fresh-number!)
]

The @litchar{!} at the end of @rhombus(fresh-number!) is a convention
to warn readers that calling the function can have a side effect.

Although you can set a variable's value using @rhombus(set!), you can't
directly pass a variable to another function that changes the
variable's value. A @rhombus(set!) on a function's argument would
change the argument variable's value, but would have no effect on the
caller's variables. To make a mutable location that can be passed
around, Shplait supports @defterm{boxes}. You can think of a box as a
mutable object that has a single field, where @rhombus(box) creates a
fresh object, @rhombus(unbox) extracts the object's field, and
@rhombus(set-box!) changes the object's field.

@interaction[
(define counter1 (box 0))
(define counter2 (box 0))
(define (fresh-number-at! c)
  (begin
    (set-box! c (add1 (unbox c)))
    (unbox c)))
(fresh-number-at! counter1)
(fresh-number-at! counter1)
(fresh-number-at! counter2)
(fresh-number-at! counter1)
]

A @defterm{vector} is a traditional mutable array. Every element of a
vector must have the same type, which can be inferred from the value
that you suply when making a vector to serve as the initial value for
each of the vector's slots. The @rhombus(vector) function creates a vector,
@rhombus(vector-ref) accesses a slot value by position, and
@rhombus(vector-set!) changes a slot value by position.

@interaction[
(define counters (make-vector 2 0))
(define (fresh-number-at-index! i)
  (begin
    (vector-set! counters i (add1 (vector-ref counters i)))
    (vector-ref counters i)))
(fresh-number-at-index! 0)
(fresh-number-at-index! 0)
(fresh-number-at-index! 1)
(fresh-number-at-index! 0)
]


@; ----------------------------------------

*/)
