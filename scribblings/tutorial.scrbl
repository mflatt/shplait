#lang scribble/rhombus/manual
@(import:
    rhombus/runtime_path
    rhombus/meta open
    meta_label:
      only_meta 0:
        shplait open
    "eval.rhm".eval
    "spacer.rhm" open
    scribble/private/typeset_meta
    lib("scribble/core.rkt") as s_core
    lib("scribble/html-properties.rkt") as html_prop
    "tutorial_url.rhm" open
    lib("scribble/manual.rkt").racketresultfont)

@(def defterm = italic)

@(def ref_scrbl = ModulePath 'lib("scribblings/reference/reference.scrbl")')
@(def shrubbery_scrbl = ModulePath 'lib("shrubbery/scribblings/shrubbery.scrbl")')

@(fun pink(e):
    elem(~style: s_core.style(#false, PairList[s_core.#{background-color-property}("pink")], e)))

@(runtime_path.def demo_rhm: "demo.rhm")
@(def demo_link:
    @elem(~style: s_core.style(#false, PairList[html_prop.#{link-resource}(demo_rhm)])){@filepath{demo.rhm}})

@(macro 'interaction($term, ...)': 'examples(~eval: eval, $term, ...)')

@(def quotes = @elem{@litchar{'}…@litchar{'}})
@(def brackets = @elem{@litchar{[}…@litchar{]}})

@(defn.macro 'meta_var $id':
    'meta.bridge $(typeset_meta.in_space(id)):
       typeset_meta.make_Transformer(
         fun (stx):
           'elem(rhombus($id, ~var))'
       )')
@(meta_var variant_name_1)
@(meta_var variant_name_2)
@(meta_var field_name_1)
@(meta_var field_name_2)
@(meta_var field_name_3)
@(meta_var field_name_4)
@(meta_var Type_1)
@(meta_var Type_2)
@(meta_var Type_3)
@(meta_var Type_4)

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

In other words, the expression @rhombus(1) has type @rhombus(Int),
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

Shplait supports various kinds of integers, all with type @rhombus(Int):

@interaction(
  1
  -42
  1_000_000
)

The full syntax of numbers is probably not important, but it's
@seclink(~doc: shrubbery_scrbl, "top"){shrubbery number syntax}
restricted to integers.

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
allowed in operator names.

Individual characters are infrequently used in Shplait, and they do not
have a convenient syntax, but they can be written with @litchar{#\}
inside @litchar{#{}}:

@interaction(
  #{#\a}
  #{#\A}
  #{#\space} // same as #\ followed by a space
)

@// ----------------------------------------
@section(~tag: "built-ins-tutorial"){Using Built-in Operators and Functions}

Shplait includes some of the usual operators and functions on numbers, like
@rhombus(+) and @rhombus(max), used in the usual way.

@interaction(
  1 + 2
  max(3, 5)
  ~error:
    1/0
)

Extra parentheses are allowed around an expression.

@interaction(
  (((1) + ((2))))
)

The type of a function is written with @rhombus(->, ~at shplait/type). The function's
argument types appear before the arrow, and the function's result type
is after the arrow. If a function only has one argument, the argument
type can be written by itself, otherwise the argument types are
comma-separated and grouped with parentheses. A function is a value, so
if you evaluate just @rhombus(max) without calling
it, then Shplait will show the type and print that the result is a
function:

@interaction(
  max
)

Line breaks and indentation matter in Shplait. A @rhombus(1 +) with
@rhombus(2) on the next line does not count as a use of the @rhombus(+) operator:

@interaction(
  ~error:
    :
      1 +
      2
)

An expression with infix operator can be split across lines, as long as
the operator appears at the start of the second line, and as long as the
second line is more indented than the first.

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

Lines and indentation still matter within parentheses. Function-call
arguments can be on their own lines, but any argument that starts a new
line must be indented the same as other arguments that start on their
own lines. Indentation inside of parentheses can start anywhere, and it
is not constrained by indentation outside the parentheses; usually, it
starts either just after the open parenthesis by continuing on that same
line, or it starts on a new line that is indented relative to the
preceding line (instead of the preceding parenthesis).

@interaction(
  max(1,
      2)
  max(
    1,
    2
  )
)

Here are some example uses of other built-in operators and functions,
and you can click on any of the operator or function names to jump to
the documentation:

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
the type of @rhombus(==) by a symbol placeholder @rhombus(?a, ~at shplait/type), which
you can read as ``a type to be picked later.'' A specific type is
picked for every individual use of @rhombus(==).

@interaction(
  1 == 1
  #'a == #'b
  ~error:
    1 == "a"
)

@// ----------------------------------------
@section(~tag: "cond-tutorial"){Conditionals}

An @rhombus(if) has a ``test'' expression followed by a vertical bar
@litchar{|} before the ``then'' expression, and then another vertical
bar @litchar{|} before the ``else'' expression. An ``else'' expression
is required.

@interaction(
  if "apple" == "banana" | #'yes | #'no
  ~error:
    if "apple" == "banana" | #'yes
)

A @litchar{|} can start on its own line. If the first @litchar{|} starts
on its own line, then the @litchar{|} must be vertically aligned with
@rhombus(if). A later @litchar{|} on its own line must be aligned with
the first @litchar{|}.

@interaction(
  if "apple" == "banana"
  | #'yes
  | #'no
  if "apple" == "banana" | #'yes
                         | #'no
)

DrRacket can help you indent by the right amount. Hit the Tab key to cycle
through syntactically valid indentations for the current line. Note that
if your line doesn't yet start with @litchar{|}, then DrRacket will
offer different choices than when the line does start with @litchar{|}.

The @rhombus(cond) form is a multi-way @rhombus(if). A @rhombus(cond)
form has a sequence of clauses, each starting with @litchar{|}. A clause
has a ``question'' and a result expression, and those two parts are
separated with a colon @litchar{:}. When @rhombus(cond) runs, the result
expression is used only when the question expression produces
@rhombus(#true). The @rhombus(cond) form tries the clauses in order, and
as soon as it finds a @rhombus(#true) result from a question, it returns
the corresponding result. If there's no @rhombus(#true) result from any
question, and error is reported. The last clause's question can be
@rhombus(~else) as a synonym for @rhombus(#true).

@interaction(
  cond
  | 2 < 1: 3
  | 2 > 1: 4
  cond
  | 2 < 1: 1/0 // result expression skipped
  | 2 > 1: 4
  cond
  | #true: 3
  | #true: 1/0 // second clause not reached
  cond
  | 3 < 1: 0
  | 3 < 2: 1
  | 3 < 3: 2
  | 3 < 4: 3
  ~error:
    cond
    | 2 < 1: -3
    | 2 < 0: 1-4
  cond
  | "a" == "b": 0
  | "a" == "c": 1
  | ~else: 2
)

The rules for starting a @litchar{|} on a new line are the same for any
form, whether it's @rhombus(if), @rhombus(cond), or something else. So,
a @rhombus(cond) form could be written on a single line, although that's
usually not as readable.

@interaction(
  cond | "a" == "b": 0 | "a" == "c": 1 | ~else: 2
)

General rules also apply to newlines @emph{after} @litchar{:}. The
content after a @litchar{:} can start on its own line, as long as it's
more indented that the content before @litchar{:}. The indentation
doesn't need to be more than the @litchar{:} itself---only more than the
content before the @litchar{:}. If that content is in a @rhombus{|},
then the start of the content is after the @litchar{|}.

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
are @defterm{short-circuiting}, which means that they stop as soon as a
@rhombus(#true) or @rhombus(#false) result is determined.

@interaction(
  #true && #true
  #true && #false
  2 < 1 && #true
  2 < 1 && 1/0 == 0 // second expression is not evaluated
  #false || #true
  #false || #false
  1 < 2 || 1/0 == 0 // second expression is not evaluated
  #true && #true && #true
  #false || #false || #false
)

@// ----------------------------------------
@section(~tag: "lists-tutorial"){Lists}

Shplait lists are @defterm{uniform}, meaning that all of the elements of a
list must have the same type. Square brackets @brackets create a list
with comma-separated elements.

@interaction(
  [1, 2, 3 + 4]
  [string_append("a", "b"), "c"]
)

The newline and indentation rules inside @brackets are the same as
inside parentheses, so when an element of the list starts on a new line,
it must be vertically aligned with the first element of the list.

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

As you can see from the examples, the type of a list is written as
@rhombus(Listof, ~at shplait/type) followed by parentheses containing
the type of the list's elements.

A list is immutable. That is, the value @rhombus([1, 2, 3]) is as
unchanging as the numbers @rhombus(1), @rhombus(2), and @rhombus(3)
within the list. You can't change a list to add new elements to
it---but you can create a new list that is like the old one, except
that it has another element. The @rhombus(cons) function takes an
element and a list and adds the element to the front of the list,
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

Although the multi-element @brackets form is convenient, the true
list-construction primitives are just @rhombus([]) and @rhombus(cons),
and you can build up any other list using those:

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
@rhombus(rest) gets everything in the list with the first item removed.

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
known shape. More commonly, a function that takes a nonempty list will
need to use @rhombus(first) and recur with the @rhombus(rest). Here's a
function that checks whether @rhombus("milk") is in a list of strings
(but we explain more about definitions in the next section):

@interaction(
  ~defn:
    fun got_milk(items :: Listof(String)) :: Boolean:
      cond
      | is_empty(items): #false
      | is_cons(items):
          first(items) == "milk" || got_milk(rest(items))
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
 def pi = 3 // close enough, since we only have integers
 pi
 def tau = pi + pi
 tau
)

You can use either @litchar{=} or @litchar{:} to separate the defined
name from its value. Use @litchar{=} when everything is on one line, and
use @litchar{:} (with the usual rule for indentation after @litchar{:})
when multiple lines are needed.

The @rhombus(fun) form defines a function. The new function's name is
followed by an open parenthesis, a comma-separated sequence of argument
names, and a closing parenthesis. A @litchar{:} separates the arguments
from body of the function.

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
    fun num_is_even(x :: Int): #true
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
  num_is_odd
  num_is_odd(12)
)

In our definitions of @rhombus(pi) and @rhombus(tau), Shplait inferred
that the newly defined names have type
@rhombus(Int, ~at shplait/type). It also inferred that
@rhombus(num_is_odd) has type
@rhombus(Int -> Boolean, ~at shplait/type). Programs are often easier
to read and understand if you write the type that would otherwise be
inferred. Declaring types can sometimes help improve or localize error
messages when Shplait's attempt to infer a type fails, since inference
can otherwise end up depending on the whole program.

Declare a type in @rhombus(def) by writing @rhombus(::) and a type after
the defined identifier:

@interaction(
  ~defn:
    def groceries :: Listof(String) = ["milk", "cookies"]
)

For a function, attach a type to an argument using @rhombus(::) plus the
type. Write the function's result type with @rhombus(::) after the
argument parentheses but before the @litchar{:} for the function's body.

@interaction(
  ~defn:
    fun starts_milk(items :: Listof(String)) :: Boolean:
      is_cons(items) && first(items) == "milk"
)

You can declare local functions and constants by using the
@rhombus(block) form. Use @litchar{:} immediately after @rhombus(block),
and then write any number of definitions and expressions, each on its
own line. The last form in a @rhombus(block) body must be an expression,
and that expression supplies the result for the whole @rhombus(block)
form.

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

The @rhombus(block) could be used inside a function to define a helper
function or to avoid a repeated computation involving the function
arguments. In cases like the body of @rhombus(fun), however, there is an
implicit @rhombus(block), so you can just write local definitions there
directly.

@interaction(
  ~defn:
    fun discard_first_if_fruit(items):
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
@rhombus(block) with just one definition. The
@rhombus(discard_first_if_fruit) example above can be equivalently
written using @rhombus(let):

@interaction(
  ~defn:
    fun discard_first_if_fruit(items):
      let a = first(items):
        cond
        | a == "apple": rest(items)
        | a == "banana": rest(items)
        | ~else: items
)

Although you can write multiple expressions in a @rhombus(block), since
those expressions other than the last one do not contribute to the
result of the block, those expressions must have type @rhombus(Void).
The intent is to allow printing or other side-effecting expressions while
disallowing expressions whose result is ignored (perhaps accidentally).

@interaction(
  ~repl:
    block:
      println("Adding...")
      1+2
    ~error:
      block:
        3+4 // not ok to ignore Int result
        1+2
)

@// ----------------------------------------
@section(~tag: "datatypes-tutorial"){Datatypes}

So far, we have only seen built-in types like
@rhombus(Int, ~at shplait/type) and
@rhombus(Listof(String), ~at shplait/type). Sometimes, it's useful to
define your own name as a shorthand for a type, such as defining
@rhombus(Groceries) to be equivalent to
@rhombus(Listof(String), ~at shplait/type):

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
cases with @litchar{|}, one for each @defterm{variant} of the type, and
where each variant has some number of @defterm{fields}. The general form
is

@rhombusblock(
  type #,(@rhombus(Type, ~var))
  | variant_name_1(field_name_1 :: Type_1,
                   field_name_2 :: Type_2,
                   ...)
  | variant_name_2(field_name_3 :: Type_3,
                   field_name_4 :: Type_4,
                   ...)
  | ...
)

If you're used to Java-style classes, you can think of
@rhombus(Type, ~var) as an interface, and each variant is a class that
implements the interface. Unlike Java classes, a variant name doesn't
work as a type name; it only works to create an instance of the variant.

For example, the following definition is suitable for representing
animals that can be either tigers or snakes:

@interaction(
  ~defn:
    type Animal
    | tiger(color :: Symbol,
            stripe_count :: Int)
    | snake(color :: Symbol,
            weight :: Int,
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

 @item{@rhombus(#,@(rhombus(expr, ~var)) is_a tiger) for an
  @rhombus(expr, ~var) that produces an @rhombus(Animal) determines
  whether the @rhombus(Animal) value was created by @rhombus(tiger) (as
  opposed to @rhombus(snake));}

 @item{@rhombus(#,@(rhombus(expr, ~var)) is_a snake) similarly
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
well-typed as far as Shplait can tell, since @rhombus(tiger.color) wants
an @rhombus(Animal) and @rhombus(slimey) has type @rhombus(Animal).

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
  | variant_name_1(field_name_1, field_name_2, ...):
      #,(@rhombus(result_expr_1, ~var))
  | variant_name_2(field_name_3, field_name_4, ...):
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
instantiated by the result of @rhombus(val_expr, ~var). For the clause
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
DrRacket's definitions area. Then, you can mouse over the variable @litchar{a} in
@rhombus(animal_color) to confirm that it means the @rhombus(a) that is
passed as an argument. Mouse over @litchar{col} to see that it means
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
@rhombus(match) form works on lists when you use @rhombus([]) and
@rhombus(cons(fst, rst)) patterns, so here's an improved
@rhombus(got_milk):

@interaction(
  ~defn:
    fun got_milk(items :: Listof(String)):
      match items
      | []: #false
      | cons(item, rst_items):
          item == "milk" || got_milk(rst_items)
  ~repl:                   
    got_milk([])
    got_milk(["cookies", "milk"])
)

The @rhombus([]) pattern is a special case in @rhombus(match), and can
only be used with @rhombus(cons) or @rhombus(~else) in the other case.
For types that you define yourself, every pattern in @rhombus(match)
will have a variant name followed by arguments in parentheses. Even if a
variant has no fields, it will have an empty pair of parentheses in its
construction and pattern, as demonstrated by the @rhombus(incomplete)
variant in this example:

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

You can use @rhombus(~else) for a final clause in @rhombus(match)
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
message (and then continues, anyway) if the test fails.

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
    fun always_fail(n :: Int) :: Int:
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
calling. To relax this constraint, wrap tests in a @rhombus(module test)
form. A @rhombus(module test) wrapper effectively moves its content to
the end of the program.

@rhombusblock(
  :«
    module test:
      check:
        retaste("milk") // use appears before definition of retaste
        ~is [#'still, #'good]

    fun retaste(s):
      [#'still, taste(s)]

    // test module runs automatically after the definition
   »
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

When you're debugging a program, it may be helpful to see the
arguments that are passed to a particular function and the results
that the function returns. You can enable that kind of tracing for a
function with the @rhombus(trace) form. Tracing is enabled until the
expression in the body of @rhombus(trace) returns a value.

@interaction(
  ~defn:
    fun got_milk(items :: Listof(String)):
      match items
      | []: #false
      | cons(item, rst_items): item == "milk" || got_milk(rst_items)
  ~repl:
    trace got_milk:
      got_milk([])
    trace got_milk:
      got_milk(["cookies", "milk"])
)

To trace more than one function, you can nest @rhombus(trace) forms.

As you're developing a program, sometimes it's useful to run a partial
program where you haven't yet decided on part of the implementation. The
@rhombus(....) operator (with four dots) can be used as a prefix, infix, or
postfix operator. A program using @rhombus(....) can compile and run, but
the @rhombus(....) reports an error if it is reached during evaluation.

@interaction(
  ~defn:
    fun got_milk(items :: Listof(String)):
      match items
      | []: #false
      | cons(item, rst_items): .... not sure ....
  ~repl:
    got_milk([])
    ~error:
      got_milk(["cheese"])
)

@// ----------------------------------------
@section(~tag: "lambda-tutorial"){Anonymous Functions}

After we define a function, the name of the function can be used as a
value without calling it. If you just evaluate the function name, then
Shplait will print something like @nonbreaking{@racketresultfont("#<function>")}.

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

Notice that the result has a function type: it's a function that takes a
@rhombus(Int, ~at shplait/type) and returns a
@rhombus(Int, ~at shplait/type).

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

If we write @rhombus(pi * pi), then given our
@seclink("definitions-tutorial"){earlier definition} of @rhombus(pi),
the result is as you'd expect, even if we include a lot of extra space
around the @rhombus(*):

@interaction(
  pi    *  pi
)

We could add string quotes around the expression and get a
completely different result:

@interaction(
  "pi    *  pi"
)

If you're studying programming languages and building interpreters, then
a string representation of program text is potentially useful. In
particular, you might want to treat that text as being a program in a
different language than Shplait. Still, parsing the @rhombus(pi)
identifiers and @rhombus(*) operator out of that string, including
ignoring the unimportant whitespace, is a lot of work.

Shplait offers a different kind of quoting via single-quote marks
@quotes:

@interaction(
  'pi    *  pi'
)

Superficially, this interaction is similar to the one with a string, but
there are two differences: the type was reported as
@rhombus(Syntax, ~at shplait/type), and the whitespace has been
normalized. Whitespace is normalized because a @tech{syntax object} is
not just a sequence of characters, but something that is been parsed
into structured components.

When you compare syntax objects with @rhombus(==), then the comparison
is based on that structure, ignoring whitespace differences or, say, the
particular way that a number value is written.

@interaction(
  '1 + 2.0' == '1  +  2.0000'
  '1 + 2' == '3 + 0'
)

While the syntaxes @rhombus(2.0000) and @rhombus(2.0) are equivalent
ways of writing the inexact number @rhombus(2.0), note that @rhombus(==)
here is not checking whether the interpreted values of the two quoted
expressions would be the same. The syntax objects might not even be
intended as Shplait expressions. For example, maybe @rhombus('1 + 2.0')
is meant to represent a set that contains two numbers, instead of adding
them.

To give a different interpretation to a syntax object, you would need to
inspect the pieces. One way to inspect is using @rhombus(syntax_split),
which extracts the pieces of a single-line syntax object.

@interaction(
  syntax_split('pi * pi')
)

Parenthesized terms and blocks formed with @litchar{:} count as
individual terms for @rhombus(syntax_split), although they also have
nested structures.

@interaction(
  syntax_split('1 * (3 + 4)')
)

The structure implemented by syntax objects is
@seclink(~doc: shrubbery_scrbl, "top"){shrubbery notation}. Shrubbery
notation defines the syntax of numbers, operators, identifiers, and it
defines how newlines and indentation work with @litchar{|} and
@litchar{:}, but it doesn't give an interpretation to those forms.

Splitting a syntax object like @rhombus('1 2 3') produces three
syntax objects, but those syntax object are still distinct from Shplait
numbers. Clearly, there is a correspondence between the syntax object
@rhombus('1') and the number @rhombus(1), the syntax object
@rhombus('#false') and the boolean @rhombus(#false), and the syntax
object @rhombus('x') and the symbol @rhombus(#'x). Functions like
@rhombus(syntax_is_integer), @rhombus(syntax_to_integer), and
@rhombus(integer_to_syntax) let you move between program representations
as syntax object and values that you can compute with at the Shplait
level.

@interaction(
  syntax_is_integer('1')
  syntax_to_integer('1')
  integer_to_syntax(1)
  syntax_is_integer('x')
)

In principle, you can use @rhombus(syntax_split) and conversion
functions like @rhombus(syntax_is_integer) to pull apart syntax objects
in any way. That quickly gets tedious, however, and Shplait offers
better support for manipulating syntax objects with patterns and
templates, as we see in the next section.

@// ----------------------------------------
@section(~tag: "pattern+template"){Syntax Templates and Patterns}

In a @quotes expression for a syntax object, @rhombus($) acts as an
escape back to an espression that is evaluated as a Shplait expression.
That is, instead of quoting the escaped expression, the @emph{value} of
the expression is substituted into the syntax object. The @rhombus($)
escapes only the immediately following term, so a complicated expression
needs parentheses.

@interaction(
  '1 + $(integer_to_syntax(2 + 3)) + 4'
  let x = integer_to_syntax(2 + 3):
    '1 + $x + 4'
)

This kind of @quotes form is called a @defterm{template}, because it is
instantiated to a synatx object each time it is evaluated. If the
template is put inside a function, it can generate a different syntax
object each time.

@interaction(
  ~defn:
    fun make_mult(left, right):
      '$left * $right'
  ~repl:
    make_mult('1', '2')
    make_mult('x', 'y')
    make_mult('3 + 4', '5 + 6')
)

As the last example illustrates, the syntax object substituted into a
template can have multiple terms inside, although it must have a single
shrubbery group. The last example also illustrates that template
construction is oblivious to precedence rules, which are not part of the
specification of shrubbery notation (and are instead defined for
specific languages like Shplait).

Going the other way, suppose you want to check whether a syntax object
is anything with @rhombus('*') in the middle. Any such term is one that
@emph{could} have been generated with the template
@rhombus('$left * $right') for some @rhombus(left) and @rhombus(right).
In other words, we want to turn the template around and use it as a
@defterm{pattern}.

When @rhombus(match) uses a @litchar{'}-quoted form instead of a variant
constructor, then it performs syntax matching instead of datatype
matching. Within the quoted term, @rhombus($) escapes, and a variable
after @rhombus($) is bound to a matching part of the input syntax.

@interaction(
  match '3 * 4'
  | '$left * $right':
      [left, right]
)

In this example, @rhombus(left) was matched up with @rhombus(3) in the
input pattern, and @rhombus(right) was matched up with @rhombus(4), and
that assignment of variables allows @rhombus('3 * 4') to match
@rhombus('$left * $right'). The two matched syntax objects were then
returned in a list with @rhombus([left, right]), but @rhombus(left) and
@rhombus(right) could be used in other ways. Another way to use them,
for example, is to put them back into a new synatx object through a
template.

@interaction(
  ~defn:
    fun commute(exp):      
      match exp
      | '$left * $right':
          '$right * $left'
  ~repl:
    commute('1 * 2')
    commute('1 + 2 * 3 + 4')
)

As the last call to @rhombus(commute) illustrates, again, matching and
template construction manipulate terms sequences with no regard to
meaning or precedence beyond the rules that shrubbery notation defines.

Escapes in a syntax pattern can be nested so that they stand for more
deeply nested parts of a matching syntax object.

@interaction(
  ~defn:
    fun check_call(exp):
      match exp
      | 'f(1 + $a, 2 + $b)':
          "matches with " +& a +& " and " +& b
      | ~else:
          "doesn't match"
  ~repl:
    check_call('f(1 + 3, 2 + 4 + 5)')
    check_call('3')
    check_call('f(1, 2)')
)

What if we want to match a function-call shape with any number of
arguments, instead of just two? A pattern variable can match any number
of terms in a row, but @litchar{,} in shrubbery notation separates
groups, so @rhombus('f($args)') cannot match @rhombus('f(1, 2, 3)'),
because the @rhombus(1), @rhombus(2), and @rhombus(3) are in different
groups. To support more general sequence matching, patterns and
templates recognize @litchar{...} to mean zero or more repetitions of
the group or term before the @litchar{...}. So, the pattern
@rhombus('f($arg, ...)') does match @rhombus('f(1, 2, 3)'). 

@interaction(
  ~defn:
    fun check_any_call(exp):
      match exp
      | 'f($arg, ...)':
          "matches"
      | ~else:
          "doesn't match"
  ~repl:
    check_any_call('f(1)')
    check_any_call('f(1, 2, 3)')
    check_any_call('f()')
    check_any_call('f')
)

When @rhombus('f($arg, ...)') matches @rhombus('f(1, 2, 3)'), the
variable @rhombus(arg) doesn't just stand for one piece of the input
syntax object. It needs to stand for all of the matches, and the only
way to get at those matches is to use the variable in a template that
also uses @litchar{...}.

@interaction(
  ~defn:
    fun show_call_args(exp):
      match exp
      | 'f($arg, ...)':
          "matches with " +& '[$arg, ...]'
  ~repl:
    show_call_args('f(1, 2, 3)')
)

The template @rhombus('[$arg, ...]') creates a syntax object that isn't
the same as a list, but it corresponds to a list. Use the function
@rhombus(syntax_to_list) to convert it to a list of syntax objects.

@interaction(
  ~defn:
    fun get_call_args(exp):
      match exp
      | 'f($arg, ...)':
          syntax_to_list('[$arg, ...]')
  ~repl:
    get_call_args('f(1, 2, 3 + 4)')
    get_call_args('f()')
)

@// ----------------------------------------
@section(~tag: "tuples-tutorial"){Tuples and Options}

If you want to combine a small number of values in a single value, and
if the values have different types (so that a list doesn't work), you
can use a @defterm{tuple} as an alternative to creating a new datatype
with a single variant.

The @rhombus(values) form creates a tuple from any number of values.
The type of a tuple reveals the type of every component value in the
tuple, separating the types with @rhombus(*, ~at shplait/type).

@interaction(
  values(1, "milk", #'apple)
  values([1, 2, 3], #false)
)

Using @rhombus(values), this @rhombus(consume) function can effectively
return two values each time that it is called:

@interaction(
  ~defn:
    fun consume(s :: String) :: (Symbol * String):
      cond
      | s == "milk": values(#'drink, "Mmm....")
      | s == "beets": values(#'eat, "Ugh....")
      | ~else: values(#'sleep, "Zzz...")
  ~repl:
    consume("milk")
)

To extract the component values from a tuple, match the tuple with
names using @rhombus(def values).

@interaction(
  def values(action, response) = consume("beets")  
  action
  response
)

The convenience functions @rhombus(fst) and @rhombus(snd) can be used in
the special case of a 2-value tuple to extract the first or second
component.

@interaction(
  snd(consume("milk"))
)

Sometimes, instead of always returning multiple values, you'll want a
function that returns either one value or no value. A tuple is no help
for that case, but Shplait predefines a helpful datatype called
@rhombus(Optionof, ~at shplait/type):

@rhombusblock(
  type Optionof(?a)
  | none()
  | some(v :: ?a)
)

The @rhombus(?a, ~at shplait/type) in this definition of
@rhombus(Optionof, ~at shplait/type) indicates that you get to pick the
type of value every time that you use @rhombus(some).

@interaction(
  ~defn:
    fun get_slogan(s :: String) :: Optionof(String):
      cond
      | s == "milk": some("It does a body good")
      | ~else: none()
  ~repl:
    get_slogan("milk")
    get_slogan("iced tea")
    match get_slogan("moon pie"):
    | some(s): s
    | none(): "no comment"
)

@// ----------------------------------------
@section(~tag: "program-tutorial"){Programs and Modules}

When you write a program using
@rhombus(#,(hash_lang()) #,(rhombuslangname(shplait))), you are
technically defining a @defterm{module}. A Shplait module contains a
mixture of expressions and definitions. The expressions are evaluated in
order, and the value of each expression is printed after the expression
is evaluated (unless the result value has type
@rhombus(Void, ~at shplait/type)). The order of function definitions
doesn't matter, as long as a function definition appears before any
expression that eventually calls the function.

@rhombusblock(
  #,(hash_lang()) #,(rhombuslangname(shplait))

  fun num_is_odd(x):
    if x == 0
    | #false
    | num_is_even(x-1)

  num_is_odd(0) // ok
  #//
  num_is_odd(1) // won't work, because it needs num_is_even

  fun num_is_even(x):
    if x == 0
    | #true
    | num_is_odd(x-1)

  num_is_even(1) // ok
  num_is_odd(1) // ok
)

Note the use of @litchar{#//} in the example above. A @litchar{#//}
comments out the entire form that follows it, which is handy for
commenting out a definition or expression, even when the definition or
expression spans multiple lines.

Modules written with the @rhombus(module) form can be nested in other
modules. A nested module is called a @defterm{submodule}. More precisely, the
@rhombus(module) form creates a submodule by merging all
@rhombus(module)s that use the same name. A typical use of
@rhombus(module) is to move all of a program's tests into a
@rhombus(test) submodule.

@rhombusblock(
  #,(hash_lang()) #,(rhombuslangname(shplait))

  fun num_is_odd(x):
    if x == 0
    | #false
    | num_is_even(x-1)

  module test:
    num_is_odd(0) // ok
    num_is_odd(1) // ok

  fun num_is_even(x):
    if x == 0
    | #true
    | num_is_odd(x-1)

  module test:
    num_is_even(1) // ok
    num_is_odd(1) // ok
)

The submodule name @rhombus(test) is special, because DrRacket
automatically runs a @rhombus(test) submodule (if one is present) after
running the enclosing module. In the above example, since the
@rhombus(test) submodule is run after the encloding module that defines
@rhombus(num_is_odd) and @rhombus(num_is_even), the tests can use all of the
functions. Another advantage of putting tests in a @rhombus(test)
submodule is that you can turn off the tests. In DrRacket's
@onscreen{Language} menu, select @onscreen{Choose Language}, click
@onscreen{Show Details}, click @onscreen{Submodules to run}, and then
uncheck the @onscreen{test} item.

A Shplait module's definitions are automatically exported from the
module. You can import the definitions of another module by using the
@rhombus(import) form, typically with a string that is a relative path
to the module to import. To access the imported names, use the file name
plus @litchar{.} as a prefix, or add @rhombus(open) before the file name
to make imported names visible without a prefix.

@rhombusblock(
  // math.rhm
  #,(hash_lang()) #,(rhombuslangname(shplait))

  def pi = 3 // we still only have integers
  def tau = pi + pi
)

@rhombusblock(
  // circle.rhm
  #,(hash_lang()) #,(rhombuslangname(shplait))
  import:
    "math.rhm"

  fun circle_area(r):
    math.pi * r * r
  fun circumference(r):
    r * math.tau
)

@rhombusblock(
  // sphere.rhm
  #,(hash_lang()) #,(rhombuslangname(shplait))
  import:
    open: "math.rhm"

  fun sphere_volume(r):
    (4/3) * pi * r * r * r
)

A submodule created by @rhombus(module) automatically imports the
bindings of the enclosing module, which is why @rhombus(module test)
submodules can automatically access definitions for testing. In
contrast, if you write definitions inside @rhombus(module test), then
the definitions can be used for tests in any @rhombus(module test), but
the enclosing module will not see the definitions.

@// ----------------------------------------
@section(~tag: "state-tutorial"){State}

@nested(~style: #'inset){ @bold{Warning:} If you are using Shplait with
 a programming-languages course, then the instructor has almost certainly
 disallowed the constructs in this chaper for use in your homework
 solutions, except as specifically allowed. Don't use @rhombus(mutable),
 @rhombus(:=), @rhombus(block) with more than one body expression, boxes,
 or arrays unless the instructor says that you can. If you're tempted to
 use one of those, you're doing it wrong.}

We have so far described @rhombus(def) as naming constants, but names
bound by @rhombus(def mutable) can be modified after the definition. The
value associated to the name can be changed using the @rhombus(:=)
operator.

@interaction(
  ~defn:
    def mutable gravity = 66
  ~repl:
    gravity
    gravity := 65
    gravity
)

The type of a @rhombus(:=) expression is
@rhombus(Void, ~at shplait/type), meaning that it doesn't return a
useful value, and the useless value doesn't even print as a result. If
you need to change a variable and then return a value, use
@rhombus(block) to sequence the operations. The value of a
@rhombus(block) form is the value of its last expression.

@interaction(
  ~defn:
    def mutable counter = 0

    fun fresh_number():
      block:
        counter := counter + 1
        counter
  ~repl:
    fresh_number()
    fresh_number()
)

Although you can set a variable's value using @rhombus(:=), you can't
directly pass a variable to another function that changes the variable's
value. Even if it were allowed syntactically, a @rhombus(:=) on a
function's argument would change the argument variable's value, but
would have no effect on the caller's variables. To make a mutable
location that can be passed around, Shplait supports @defterm{boxes}.
You can think of a box as a mutable object that has a single field,
where @rhombus(box) creates a fresh object, @rhombus(unbox) extracts the
object's field, and @rhombus(set_box) changes the object's field.

@interaction(
  ~defn:
    def counter1 = box(0)
    def counter2 = box(0)
    fun fresh_number_at(c):
      block:
        set_box(c, unbox(c) + 1)
        unbox(c)
  ~repl:
    fresh_number_at(counter1)
    fresh_number_at(counter1)
    fresh_number_at(counter2)
    fresh_number_at(counter1)
)

An @defterm{array} is a traditional mutable array. Every element of an
array must have the same type, which can be inferred from the value that
you supply when making an array to serve as the initial value for each of
the array's slots. The @rhombus(make_array) function creates an array,
@brackets can be used after an array expression to access a slot of the
array by position, and @brackets plus @rhombus(:=) changes a slot value
by position.

@interaction(
  ~defn:
    def counters = make_array(2, 0)
    fun fresh_number_at_index(i):
      block:
        counters[i] := counters[i] + 1
        counters[i]
  ~repl:
    fresh_number_at_index(0)
    fresh_number_at_index(0)
    fresh_number_at_index(1)
    fresh_number_at_index(0)
)

@// ----------------------------------------
