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

@title{Splait}

Shplait is a language for implementing interpreters.

@docmodule(~lang, shplait)


@table_of_contents()

@// ------------------------------------------------------------
@section{Syntactic Categories}

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
  identifier followed by @rhombus(::) and a declared type.}

)

}

@// ------------------------------------------------------------
@section(~tag: "sec:type"){Types}

@doc(
  type 'Number'
  type 'String'
  type 'Boolean'
){

 Types for primitive values.

}


@doc(
  type 'Listof($type)'
){

 The type of a list whose elements have type @rhombus(type).

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


@doc(
  type '#' $id'
){

 A type variable, which stands for a type to be chosen later. For
 example, @rhombus(#'a -> #'a) is the type of an identity function that
 accepts any value and returns the same value.

}

@// ------------------------------------------------------------
@section(~tag: "sec:defn"){Definitions and Functions}

@doc(
  defn.macro 'def $typed_id = $expr'
  defn.macro 'def $typed_id:
                $defn
                ...
                $expr'
){

  A definition of @rhombus(id) to the result of @rhombus(expr).

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
    variant_id: block id
    of_id: block id
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

 Each @rhombus(variant_id) is defined as a constructor function, which
 takes arguments according to the @rhombus(typed_id) field declarations
 and produces a value of type @rhombus(id) or @rhombus(id(type, ...)).

 When a @rhombus(id(type, ...)) is defined, then @rhombus(id) is a
 polymorphic type constructor. The constructors are polymorphic on the
 the degree that @rhombus(type) forms in the constructor
 @rhombus(typed_id)s refer to the @rhombus(of_id) type variables in
 @rhombus(maybe_type_args).

}


@// ------------------------------------------------------------
@section(~tag: "sec:arith"){Arithmetic and Equality}

@doc(
  expr.macro '$expr + $expr'
  expr.macro '$expr - $expr'
  expr.macro '$expr * $expr'
  expr.macro '$expr / $expr'
){

 Arithmetic.

}

@doc(
  expr.macro '$expr == $expr'
  expr.macro '$expr != $expr'
){

 Compares any two values for (in)equality, as long as the
 @rhombus(expr)s have the same type.

}

@doc(
  expr.macro '$expr < $expr'
  expr.macro '$expr > $expr'
  expr.macro '$expr <= $expr'
  expr.macro '$expr >= $expr'
){

 Number comparisons.

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
 must produce a syntax object, and it is compared to the quoted
 @rhombus(pattern)s until a match is found. A pattern can include an
 escaped with @rhombus($) to bind an identifier to the corresponding
 piece of syntax, and it can include @rhombus(...) for repetitions; when
 a @rhombus($) escape is repeated with @rhombus(...), the escaped
 variable is bound to a list of (list of ...) matches.

}

