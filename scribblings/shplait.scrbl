#lang scribble/rhombus/manual
@(import:
    "typeset.rkt" open
    meta_label:
      shplait open)

@(nonterminal:
    id: block
    type: block
    typed_id: block
    expr: block
    defn: block)

@title{Shplait}

Shplait is a language for implementing interpreters.

@docmodule(~lang, shplait)


@table_of_contents()

@// ------------------------------------------------------------
@section{Notation}

Shplait syntax is based on @deftech{shrubbery} syntax, which is
described at
@secref(~doc: [#'lib, "shrubbery/scribblings/shrubbery.scrbl"], "top").
Superficially, Shplait resembles
@secref(~doc: [#'lib, "rhombus/scribblings/rhombus.scrbl"], "top"),
which also uses shrubbery notation, but Shplait is statically typed,
much smaller, and has a different set of constructs overall.

Shrubbery notation defines the syntax of identifiers and numbers, and it
defines how token sequences are grouped by parentheses, brackets,
braces, quotes, and indentation. We define Shplait using patterns over
Shrubbery forms.

In the pattern form a syntactic form, @litchar{...} indicates zero or
more repetitions of the preceding form. The preceding form can be an
individual term, a group, or an alternative. (Words like @italic{term},
@italic{group}, and @italic{alternative} are defined as part of the
specificaiton of shrubbery notation.)

@doc(
  ~nonterminal_key: block
  grammar id
  grammar expr
  grammar defn
  grammar type

  grammar typed_id:
    id
    id :: type
){

 In syntax descriptions:

@itemlist(

  @item{@rhombus(id) stands for an identifier, such as
  @rhombus(x, ~datum) or @rhombus(interp, ~datum).}

  @item{@rhombus(expr) stands for an expression, such as
  @rhombus(x, ~datum), @rhombus("hello"), @rhombus(1 + 2), or
  @rhombus(f(3, 4)).}

  @item{@rhombus(defn) stands for an definition, such as
  @rhombus(def x = 1) or @rhombus(fun f(x): x).}

  @item{@rhombus(type) stands for any type, such as
  @rhombus(Number, ~at shplait/type).}

  @item{@rhombus(typed_id) stands for either an identifier or an
  identifier followed by @rhombus(::) and a declared type. Note that there
  cannot be spaces between the two @litchar{:}s in @litchar{::}.}

)

}

@// ------------------------------------------------------------
@section(~tag: "sec:type"){Types}

Every Shplait expression has a type. When you don't annotation a binding
or expression with a type, then Shplait infers one that you could have
written.

There are some built-in types like @rhombus(Number, ~at shplait/type),
some built-in type constructors like @rhombus(->, ~at shplait/type),
and new types can be defined with @rhombus(type).

@doc(
  type '#' $id'
){

 A type variable, which stands for a type to be chosen later. For
 example, @rhombus(#'a -> #'a) is the type of an identity function that
 accepts any value and returns the same value.

}

@doc(
  ~nonterminal:
    variant_id: block id
    field_id: block id
    of_id: block id
    as_type: block type
  decl.macro 'type $id = $as_type'
  decl.macro 'type $id $maybe_type_args
              | $variant_id ($typed_id, ...)
              | ...'              
  grammar maybe_type_args:
    ϵ
    (#' $of_id, #' $of_id, ...)
){

 Defines a new type, either @rhombus(id) or or @rhombus(id(type, ...)).
 A plain @rhombus(id) type is defined when @rhombus(maybe_type_args) is
 empty.

 Using @rhombus(type) with @rhombus(=) defines @rhombus(id) as an alias
 for @rhombus(as_type).

 Each @rhombus(variant_id) is defined as a constructor function, which
 takes arguments according to the @rhombus(typed_id) field declarations
 and produces a value of type @rhombus(id) or @rhombus(id(type, ...)).

 See the @rhombus(match) form for dispatching on variants of a type with
 pattern matching to extract field values. As an alternative,
 @rhombus(variant_id.field_id) can be used as an accessor function,
 where @rhombus(field_id) is the identifier within a field's
 @rhombus(typed_id); the accessor takes an instance of the variant and
 extracts the corresponding field value, and it raises an exception when
 applied to value (of an expression) of type @rhombus(id) that is not an
 instance of @rhombus(variant_id).

 When a @rhombus(id(type, ...)) is defined, then @rhombus(id) is a
 polymorphic type constructor, and the corresponding field-accessor
 functions are also polymorphic. These are polymorphic only to the degree
 that @rhombus(type) forms in the constructor @rhombus(typed_id)s refer
 to the @rhombus(of_id) type variables in @rhombus(maybe_type_args).

}


@// ------------------------------------------------------------
@section(~tag: "sec:defn"){Definitions and Functions}

@doc(
  defn.macro 'def $typed_id = $expr'
  defn.macro 'def $typed_id:
                $defn
                ...
                $expr'
  defn.macro 'def values($typed_id, ...) = $expr'
  defn.macro 'def values($typed_id, ...):
                $defn
                ...
                $expr'  
){

 A definition of one @rhombus(typed_id) to the result of @rhombus(expr),
 or to multiple @rhombus(typed_id)s to the components of the @tech{tuple} result
 of @rhombus(expr),

}


@doc(
  ~nonterminal:
    fun_expr: block expr
    arg_expr: block expr  
  defn.macro 'fun $id ($typed_id, ...) $maybe_type:
                $defn
                ...
                $expr'
  expr.macro 'fun ($typed_id, ...) $maybe_type:
                $defn
                ...
                $expr'

  grammar maybe_type:
    ϵ
    :: $type
){

 The @rhombus(fun, ~defn) form is a definition when @rhombus(id) appears
 immediately after @rhombus(fun, ~defn), otherwise it's an expression
 form. Each of the @rhombus(typed_id) arguments optionally declares a
 type for the argument, and if @rhombus(maybe_type) is not empty, it
 @rhombus(type) declares the function's result type.

 A function is called through the expression form
 @rhombus(fun_expr(arg_expr, ...)), where @rhombus(fun_expr) is typically
 an identifier defined with @rhombus(fun), but it can be any expression
 that produces a function. A function call of the form
 @rhombus(fun_expr(arg_expr, ...)) is an implicit use of
 @rhombus(#%call).

}

@doc(
  ~nonterminal:
    fun_expr: block expr
    arg_expr: block expr
  expr.macro '$fun_expr #%call ($arg_expr, ...)'
){

 A function call. Typically, @rhombus(#%call) is not written, but an
 expression of the form @rhombus(fun_expr(arg_expr, ...)) is an implicit
 use of @rhombus(#%call).

}

@doc(
  ~nonterminal:
    arg_type: block type    
    result_type: block type
  type '$arg_type -> $result_type'
  type '($arg_type, ...) -> $result_type'
){

 The type of a function. The @rhombus(arg_type)s specify the types of
 arguments, while @rhombus(result_type) is the type of the result.

 The @rhombus(->, ~at rhombus/type) operator associates to the right, so
 @rhombus(a -> b -> c) is the type of a function that takes @rhombus(a)
 and returns a function of type @rhombus(b -> c).

}

@// ------------------------------------------------------------
@section(~tag: "sec:cons"){Conditionals and Matching}

@doc(
  ~nonterminal:
    test_expr: block expr
    then_expr: block expr
    else_expr: block expr
  expr.macro 'if $test_expr
              | $then_expr
              | $else_expr'
){

 Conditional.

}

@doc(
  ~nonterminal:
    test_expr: block expr
    then_expr: block expr
    else_expr: block expr
  expr.macro 'cond
              | $test_expr: $then_expr
              | ...
              | ~else: $else_expr'
  expr.macro 'cond
              | $test_expr: $then_expr
              | ...'
){

 Multi-arm conditional, where @rhombus(test_expr)s are evaluated in
 order until a true result is found. If no @rhombus(~else) case is
 provided and all @rhombus(test_expr)s produce false, an error is
 reported.

}

@doc(
  ~nonterminal:
    variant_id: block id
    field_id: block id
    first_id: block id
    rest_id: block id
    target_expr: block expr
    empty_expr: block expr
    cons_expr: block expr
    result_expr: block expr
    else_expr: block expr
  expr.macro 'match $target_expr
              | $variant_id($field_id, ...): $result_expr
              | ...'
  expr.macro 'match $target_expr
              | $variant_id($field_id, ...): $result_expr
              | ...
              | ~else: $else_expr'
  expr.macro 'match $target_expr
              | []: $empty_expr
              | cons($first_id, $rest_id): $cons_expr'
  expr.macro '«match $target_expr
               | '$pattern': $result_expr
               | ...»'
  expr.macro '«match $target_expr
               | '$pattern': $result_expr
               | ...
               | ~else: $else_expr»'
){

 Pattern-matching case dispatch on the result of @rhombus(target_expr),
 either for variants of a type defined with @rhombus(type, ~defn), for
 empty or nonempty lists, or for syntax patterns.

 The most common us of @rhombus(match) is the @rhombus(variant_id) form,
 with or without @rhombus(~else). All of the @rhombus(variant_id)s must
 be for the same type, and the type of @rhombus(target_expr) ust match
 that type. If @rhombus(~else) is not present, every variant associated
 with the type must have a case.

 In the list form of @rhombus(match), the @rhombus([]) and
 @rhombus(cons) clauses can actually be in either order, and a second one
 can be replaced with @rhombus(~else).

 In the syntax-pattern form of @rhombus(match), @rhombus(target_expr)
 must produce a @tech{syntax object}, and it is compared to the quoted
 @rhombus(pattern)s until a match is found. A pattern can include an
 escaped with @rhombus($) to bind an identifier to the corresponding
 piece of syntax, and it can include @rhombus(...) for repetitions; when
 a @rhombus($) escape is repeated with @rhombus(...), the escaped
 variable is bound to a list of (list of ...) matches.

}

@// ------------------------------------------------------------
@section(~tag: "sec:builtin"){Predefined Types and Functions}

@local_table_of_contents()

@// ------------------------------------------------------------
@subsection(~tag: "sec:number"){Numbers}

@doc(
  type 'Number'
){

 The type for expressions that produce numbers.

}


@doc(
  expr.macro '$expr + $expr'
  expr.macro '$expr - $expr'
  expr.macro '$expr * $expr'
  expr.macro '$expr / $expr'
){

 Arithmetic on @rhombus(expr)s of type
 @rhombus(Number, ~at shplait/type), and the overall arithmetic
 expression also has type @rhombus(Number, ~at shplait/type).

}

@doc(
  expr.macro '$expr < $expr'
  expr.macro '$expr > $expr'
  expr.macro '$expr <= $expr'
  expr.macro '$expr >= $expr'
){

 Numeric comparison on @rhombus(expr)s of type
 @rhombus(Number, ~at shplait/type). The overall comparison expression
 has type @rhombus(Boolean, ~at shplait/type).

}

@// ------------------------------------------------------------
@subsection(~tag: "sec:boolean"){Booleans and Equality}

@doc(
  type 'Boolean'
){

 The type for expressions that produce booleans.

}

@doc(
  expr.macro '! $expr'
){

 Produces @rhombus(#true) when @rhombus(expr) produces @rhombus(#false)
 and vice versa.

}


@doc(
  expr.macro '$expr == $expr'
  expr.macro '$expr != $expr'
){

 Compares any two values for (in)equality, as long as the
 @rhombus(expr)s have the same type. The overall comparison expression
 has type @rhombus(Boolean, ~at shplait/type).

}

@// ------------------------------------------------------------
@subsection(~tag: "sec:string"){Strings}

@doc(
  type 'String'
){

 The type for expressions that produce strings.

}

@// ------------------------------------------------------------
@subsection(~tag: "sec:symbol"){Symbols}

A @deftech{symbol} is similar to a string in that it simply comprises a
sequence of characters, but a symbol expression as written with a
@rhombus(#') prefix, instead of in double quotes. Also, the character
sequence of a symbol must be valid for an identifier.

Symbols are used primarily as the representation of an identifier within
quoted code as a @tech{syntax object}. For example, the syntax object
@rhombus('f(apple)') wraps the symbols @rhombus(#'f) and
@rhombus(#'apple), while @rhombus('f("apple")') wraps a symbol
@rhombus(#'f) and a string @rhombus("apple"). The difference in those
examples is between representing a function call whose argument is the
variable @rhombus(apple) versus a function call whose argument is the
string @rhombus("apple").

@doc(
  type 'Symbol'
){

 The type for expressions that produce @tech{symbols}.

}

@// ------------------------------------------------------------
@subsection(~tag: "sec:list"){Lists}

A list has elements of a uniform type. That is, the elements of a list
can have any type, but they must all have the same type for a given
list. See also @tech{tuples}.

A list is written with square brackets, such as @rhombus([1, 2, 3]).
Using square brackets implicitly uses the @rhombus(#%brackets) form, but
@rhombus(#%brackets) is normally not written.

@doc(
  type 'Listof($type)'
){

 The type of a list whose elements have type @rhombus(type).

}

@doc(
  expr.macro '#%brackets [$expr, ...]'
){

 Produces a list whose elements in order as the values produced by the
 @rhombus(expr)s. All of the @rhombus(expr)s must have the same type.
 Normally, @rhombus(#%brackets) is omitted, since it's implied when using
 square brackets as an expression form.

}

@doc(
  fun cons(elem :: #'a, lst :: Listof(#'a)) :: #'a
  fun first(lst :: Listof(#'a)) :: #'a
  fun rest(lst :: Listof(#'a)) :: Listof(#'a)
  fun length(lst :: Listof(#'a)) :: Number
){

 The @rhombus(cons) function produces a list given its first element
 plus the rest of the elements already in a list. The @rhombus(first)
 function returns the first element of a nonempty list. The
 @rhombus(rest) funrction returns a list containing all but the first
 element of a nonempty list. The @rhombus(length) funrction returns the
 number of elements in a list.

 The @rhombus(first) and @rhombus(rest) functions raise anexception when
 given an empty list.

}

@// ------------------------------------------------------------
@subsection(~tag: "sec:tuple"){Tuples}

A @deftech{tuple} is similar to a list, but its type reflects a fixed
number of elements in the tuple, and the elements can have different
types. A tuple of one element is equivalent to just the element.

@doc(
  type '$type * $type ... * $type'
){

 The type of a tuple whose elements each have the corresponding
 @rhombus(type).

 The @rhombus(*, ~at shplait/type) type operator for tuples has higher
 precedence (i.e., joins more tightly) than the
 @rhombus(->, ~at shplait/type) type operator for functions.

}

@doc(
  expr.macro 'values($expr, ...)'
){

 Creates a tuple whose elements are produced by the @rhombus(expr)s.

}

@// ------------------------------------------------------------
@subsection(~tag: "sec:stxobj"){Syntax Objects}

A @deftech{syntax object} is a quoted term using single quotes, such as
@rhombus('1 + * 4 f()'). A syntax object is not a string, even though it
uses quote marks; instead, the usual @tech{shrubbery} rules apply inside
quotes, and shrubbery structure is preserved in a syntax object.

When a syntax object is written using quotes, the @rhombus(#%quotes)
form is used implicitly, similar to the way that square brackets
implicitly use @rhombus(#%brackets).

@doc(
  type 'Syntax'
){

 The type for expressions that produce @tech{syntax objects}.

}

@doc(
  expr.macro '«#%quotes '$term ...
                         ...'»'
){

 Produces a syntax object, quoting the @rhombus(term)s literally intead
 of treating them as subexpressions. Usually, @rhombus(#%quotes) is
 omitted, since it is implied by using quotes as an expression form.

 When @rhombus($) is used as a @rhombus(term) and isn't the only
 @rhombus(term), then it escapes so that the following @rhombus(term) is
 used as an expression after all. Typically, the escaped term is an
 identifier or a parenthesized expression. Whatever the expression
 produces is spliced into the syntax object in place of the @rhombus($)
 and escaped expression.

}


@doc(
  fun syntax_is_number(stx :: Syntax) :: Boolean
  fun syntax_is_boolean(stx :: Syntax) :: Boolean
  fun syntax_is_symbol(stx :: Syntax) :: Boolean
  fun syntax_unwrap_number(stx :: Syntax) :: Number
  fun syntax_unwrap_boolean(stx :: Syntax) :: Boolean
  fun syntax_unwrap_symbol(stx :: Syntax) :: Symbol
){

 Functions like @rhombus(syntax_is_number) check whether a @tech{syntax
  object} has a single term representing a number, returning
 @rhombus(#true) if so and @rhombus(#false) otherwise. Other functions
 check for different kinds of primitive values.

 Functions like @rhombus(syntax_unwrap_number) extract the number that
 is inside a syntax object term, but if @rhombus(syntax_is_number) would
 return @rhombus(#false), then an exception is raised.

}
