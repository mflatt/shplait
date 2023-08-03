#lang scribble/rhombus/manual
@(import:
    "typeset.rkt" open
    meta_label:
      shplait open
    "eval.rhm".eval
    "tutorial_url.rhm" open
    "spacer.rhm" open)

@(nonterminal:
    id: block
    type: block
    typed_id: block
    expr: block
    defn: block
    value: block)

@(def rhombus_scrbl = ModulePath.s_exp(ModulePath 'lib("rhombus/scribblings/rhombus.scrbl")'))

@(macro 'nodef($id)': '@rhombus($id, ~datum)')

@title{Shplait}

The Shplait language syntactically resembles the
@seclink(~doc: rhombus_scrbl, "top"){Rhombus}
language, but the type system is close to that of
@hyperlink("http://smlnj.org/"){ML}. For a quick introduction, see the
@seclink("Tutorial"){tutorial section} or the
@hyperlink(tutorial_video_url){tutorial videos}.

@docmodule(~lang, ~use_sources: shplait, shplait)

@table_of_contents()

@include_section("tutorial.scrbl")

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
specification of shrubbery notation.)

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

 These names are used in syntax descriptions, sometimes with a prefix to
 indicate a role within an enclosing form:

@itemlist(

  @item{@rhombus(id) (or a name that ends in @rhombus(id)) stands for an
  identifier, such as @rhombus(x, ~datum) or @rhombus(interp, ~datum).}

  @item{@rhombus(expr) (or a name than end in ...) stands for an
  expression, such as @rhombus(x, ~datum), @rhombus("hello"),
  @rhombus(1 + 2), or @rhombus(f(3, 4)).}

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

Every Shplait expression has a type. The types come either from
annotations or from Shplait's type inference, which fills in
missing annotations for expressions and bindings.

There are some built-in types like @rhombus(Number, ~at shplait/type),
some built-in type constructors like @rhombus(->, ~at shplait/type),
and new types can be defined with @rhombus(type).

@doc(
  type '? $id'
){

 A type variable, which stands for a type to be chosen later. For
 example, @rhombus(?a -> ?a) is the type of an identity function that
 accepts any value and returns the same value.

@examples(
  ~eval: eval
  ~defn:
    fun f(x :: ?a) :: Listof(?a):
      [x]
  ~repl:
    f(1)
    f("apple")
)

 Using a type variable does not necessarily delay the choice
 indefinitely. In some cases, type inference will resolve a type variable
 to a concrete type.

@examples(
  ~eval: eval
  ~defn:
    fun addone(x :: ?a) :: ?b:
      x+1
  ~repl:
    addone
)

}

@doc(
  ~nonterminal:
    variant_id: block id
    field_id: block id
    of_id: block id
    as_type: block type
  decl.macro 'type $id $maybe_type_args = $as_type'
  decl.macro 'type $id $maybe_type_args
              | $variant_id ($typed_id, ...)
              | ...'              
  grammar maybe_type_args:
    ϵ
    (? $of_id, ? $of_id, ...)
){

 Defines a new type, either @rhombus(id) or or @rhombus(id(#,(@nontermref(type)), ...)).
 A plain @rhombus(id) type is defined when @rhombus(maybe_type_args) is
 empty.

 Using @rhombus(type) with @rhombus(=) defines @rhombus(id) as an alias
 for @rhombus(as_type). If @rhombus(maybe_type_args) is not empty, then
 @rhombus(as_type) can refer to the arguments, and those references are
 replaced with the @rhombus(type)s supplied when @rhombus(id(type, ...))
 is used as a type. Any other type variables references in @rhombus(as_type)
 are unified across all instantiations of the type alias.

@examples(
  ~eval: eval
  ~defn:
    type NumList = Listof(Number)
  ~repl:
    def ns :: NumList = [1, 2, 3]
  ~defn:
    type Tagged(?a) = (Symbol * ?a)
  ~repl:
    def now :: Tagged(Number) = values(#'time, 1200)
    def who :: Tagged(String) = values(#'name, "Alice")
)

 When @rhombus(type) is used with @rhombus(variant_id) cases,
 each @rhombus(variant_id) is defined as a constructor function, which
 takes arguments according to the @rhombus(typed_id) field declarations
 and produces a value of type @rhombus(id) or @rhombus(id(type, ...)).

@examples(
  ~eval: eval
  ~defn:
    type Shape
    | circle(radius)
    | rectangle(width, height)
  ~repl:
    circle(2)
    rectangle(3, 4)
)

  The normal way to dispatch on variants of a type and extract
  their components is using the @rhombus(match) form. See its
  documentation for more examples:

@examples(
  ~eval: eval
  ~defn:
    def c = circle(2)
  ~repl:
    match c
    | circle(r): 3.14*r*r
    | rectangle(w, h): w*h
)

  As an alternative, @rhombus(is_a) can identify a variant, and 
  @rhombus(variant_id.field_id) can be used as an accessor function,
  where @rhombus(field_id) is the identifier within a field's
  @rhombus(typed_id).
  The accessor takes an instance of the variant and
  extracts the corresponding field value, and it raises an exception when
  applied to value (of an expression) of type @rhombus(id) that is not an
  instance of @rhombus(variant_id).


@examples(
  ~eval: eval
  ~repl:
    c is_a circle
    circle.radius(c)
)


 When a @rhombus(id(type, ...)) is defined with @rhombus(variant_id)s, then @rhombus(id) is a
 polymorphic type constructor, and the corresponding field-accessor
 functions are also polymorphic. These are polymorphic only to the degree
 that @rhombus(type) forms in the constructor @rhombus(typed_id)s refer
 to the @rhombus(of_id) type variables in @rhombus(maybe_type_args).

@examples(
  ~eval: eval
  ~defn:
    type Treeof(?a)
    | leaf(v :: ?a)
    | node(left :: Treeof(?a), right :: Treeof(?a))
  ~repl:
    node
    node(leaf(1), leaf(2))
    node(leaf("apple"), leaf("banana"))
)

}

@doc(
  ~nonterminal:
    variant_id: block id
  expr.macro '$expr is_a $variant_id'
){

 Returns @rhombus(#true) if @rhombus(expr) produces an instance
 constructed with @rhombus(variant_id), @rhombus(#false) otherwise. The
 type of @rhombus(expr) must be the same as the type produced by
 @rhombus(variant_id).

@examples(
  ~eval: eval
  ~defn:
    type Shape
    | circle(radius)
    | rectangle(width, height)
  ~repl:
    circle(2) is_a circle
    circle(2) is_a rectangle
  ~repl:
    ~error:
      "apple" is_a circle
)

}

@doc(
  type 'Optionof(?a)'
  fun none() :: Optionof(?a)
  fun some(v :: ?a) :: Optionof(?a)
){

 A type and constructors to represent ``success'' with a value and
 ``failure'' without a value, defined as

@rhombusblock(
  type Optionof(?a)
  | none()
  | some(val :: ?a)
)

}

@doc(
  expr.macro '$expr :: $type'
){

 The @rhombus(::) operator is normally used to declare a type for a
 field, variable binding, or function result, but it can also be used as
 and expression operator as long as the expression is within pareneses.

 A @rhombus(::) expression in parentheses produces the same value as
 @rhombus(expr), but also asserts that @rhombus(expr) has the type
 @rhombus(type). The type checker will report an error if the assertion
 does not hold.

 Asserting a type is potentially useful for localizing type mismatches
 that otherwise span large portions of a program.

@examples(
  ~eval: eval
  ~error:
    1:: Number
  (1 :: Number)
  ~error:
    (1 :: String)
  fun (x, y):
    // same type variable `a` forces same type for `x` and `y`
    values((x :: ?a), (y :: ?a))
)

}

@// ------------------------------------------------------------
@section(~tag: "sec:defn"){Definitions and Functions}

When defining or locally binding a name, some predefined names must be
avoided. The following names are blacklisted for binding, since
redefining or shadowing the names could easily create confusion:
@nodef(def), @nodef(fun), @nodef(block), @nodef(let), @nodef(letrec),
@nodef(if), @nodef(cond), @nodef(match), @nodef(check), @nodef(try),
@nodef(module), @nodef(import), @nodef(type), @nodef(is_a),
@nodef(Number), @nodef(Boolean), @nodef(Void), @nodef(String),
@nodef(Symbol), @nodef(Listof), @nodef(Arrayof), @nodef(Mapof),
@nodef(Syntax), @nodef(Boxof), @nodef(cons), @nodef(first),
@nodef(rest), @nodef(values), @nodef(box), @nodef(unbox),
@nodef(set_box), @nodef(Array), and @nodef(MutableMap).

@doc(
  defn.macro 'def $typed_id = $expr'
  defn.macro 'def $typed_id:
                $expr'
  defn.macro 'def values($typed_id, ...) = $expr'
  defn.macro 'def values($typed_id, ...):
                $expr'  
  defn.macro 'def mutable $typed_id = $expr'
  defn.macro 'def mutable $typed_id:
                $expr'
){

 A definition of one @rhombus(typed_id) to the result of @rhombus(expr),
 or to multiple @rhombus(typed_id)s to the components of the @tech{tuple} result
 of @rhombus(expr). When @rhombus(mutable) is specified, then the defined
 identifier's value can be changed using @rhombus(:=).

@examples(
  ~eval: eval
  ~defn:
    def x = 1
    def x2 :: Number = 2
  ~repl:
    x
    x2  
  ~defn:
    ~error:
      def s :: String = 0
  ~defn:
    def values(x3 :: Number, s2 :: String):
      values(3, "apple")
  ~defn:
    def mutable count = 0
  ~repl:
    count
    count := count + 1
    count
  )

}

@doc(
  expr.macro 'block:
                $defn_or_expr
                ...
                $expr'

  grammar defn_or_expr:
    $defn
    $expr
){

 Expression form that allows nested definitions and side-effect
 expressions before a final expression. Names defined by the
 @rhombus(defn)s are visible only within the @rhombus(block) body, and
 they shadow bindings of the same name outside of the @rhombus(block).
 Expressions among the @rhombus(defn_or_expr)s are useful only when they
 have a side effect, since their results are ignored.

@examples(
  ~eval: eval
  def x = "outside"
  x
  block:
    def x = "inside"
    println("hello")
    x
  x
)

}

@doc(
  ~nonterminal:
    fun_expr: block expr
    arg_expr: block expr  
  defn.macro 'fun $id ($typed_id, ...) $maybe_type:
                $expr'
  expr.macro 'fun ($typed_id, ...) $maybe_type:
                $expr'

  grammar maybe_type:
    ϵ
    :: $type
){

 The @rhombus(fun, ~defn) form is a definition when @rhombus(id) appears
 immediately after @rhombus(fun, ~defn), otherwise it's an expression
 form. Each of the @rhombus(typed_id) arguments optionally declares a
 type for the argument, and if @rhombus(maybe_type) is not empty, it
 declares the function's result type.

 A function is called through the expression form
 @rhombus(fun_expr(arg_expr, ...)), where @rhombus(fun_expr) is typically
 an identifier defined with @rhombus(fun), but it can be any expression
 that produces a function. A function call of the form
 @rhombus(fun_expr(arg_expr, ...)) is an implicit use of
 @rhombus(#%call).

@examples(
  ~eval: eval
  ~defn:
    fun fib(n :: Number) :: Number:
      cond
      | n == 0: 1
      | n == 1: 1
      | ~else: fib(n-1)+fib(n-2)
  ~repl:
    fib(10)
  ~defn:
    fun on_three(f :: Number -> Number) :: Listof(Number):
      [f(1), f(2), f(3)]
  ~repl:
    on_three(fun (x): x*10)
)

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

@examples(
  ~eval: eval
  ~defn:
    fun addone(x): x+1
  ~repl:
    addone #%call (0)
)

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

@examples(
  ~eval: eval
  ~repl:
    fun (x): x + 1
  ~defn:
    fun make_adder(n :: Number) :: Number -> Number:
      fun (x): x + n
  ~repl:
    make_adder
  ~defn:
    fun apply_adder(adder :: Number -> Number) :: Number:
      adder(10)
  ~repl:
    apply_adder
)

}

@doc(
  expr.macro 'let $id = $rhs_expr:
                $expr'
  expr.macro 'letrec $id = $rhs_expr:
                $expr'
){

 Shorthands for a @rhombus(block) containing a definition of
 @rhombus(id) to @rhombus(rhs_expr) followed by @rhombus(expr), except
 that in the case of @rhombus(let), @rhombus(id) is not visible to the
 @rhombus(rhs_expr).

 See @rhombus(macro, ~defn) for the definition of @rhombus(let) in terms
 of @rhombus(block) and @rhombus(def).

@examples(
  ~eval: eval
  let x = 1:
    let x = x + 2:
      x
  letrec sum = (fun(lst):
                  match lst
                  | []: 0
                  | cons(n, lst): n + sum(lst)):
    sum([1, 2, 3])
)

}

@doc(
  expr.macro '$id := $expr'
){

 Changes the value of @rhombus(id) to the result of @rhombus(expr). The
 @rhombus(id) must have been defined using @rhombus(def mutable). The
 type of @rhombus(id) and the typ eof @rhombus(expr) ust be the same.

 No result is produced by the assignment expression. That is, the type
 of the @rhombus(:=) expression is @rhombus(Void, ~at shplait/type).

@examples(
  def mutable x = 1
  x := 2
  x
)

}

@// ------------------------------------------------------------
@section(~tag: "sec:cond"){Conditionals and Matching}

@doc(
  ~nonterminal:
    test_expr: block expr
    then_expr: block expr
    else_expr: block expr
  expr.macro 'if $test_expr
              | $then_expr
              | $else_expr'
){

 A conditional form, where the type of @rhombus(test_expr) must be
 @rhombus(Boolean, ~at shplait/type) and the types of @rhombus(then_expr)
 and @rhombus(else_expr) must be the same.

@examples(
  ~eval: eval
  ~repl:
    if 1 == 2
    | "no (correct)"
    | "yes (wrong!)"
  ~repl:
    ~error:
      if 1 == 2
      | "no (correct)"
      | #'oops
)

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

@examples(
  ~eval: eval
  ~defn:
    fun count(n):
      cond
      | n == 0: "none"
      | n == 1: "one"
      | ~else: "some"
  ~repl:
    count(0)
    count(2)
)

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
  expr.macro '«match $target_expr
               | $integer_case: $result_expr
               | ...»'
  expr.macro '«match $target_expr
               | $integer_case: $result_expr
               | ...
               | ~else: $else_expr»'
  grammar integer_case:
    $integer
    $integer #,(@rhombus(||, ~datum)) $integer_case
){

 Pattern-matching case dispatch on the result of @rhombus(target_expr),
 either for variants of a type defined with @rhombus(type, ~decl), for
 empty or nonempty lists, for syntax patterns, or for integer cases.

 The most common us of @rhombus(match) is the @rhombus(variant_id) form,
 with or without @rhombus(~else). All of the @rhombus(variant_id)s must
 be for variants of the same type, and the type of @rhombus(target_expr) must match
 that type. If @rhombus(~else) is not present, every variant associated
 with the type must have a case. In the unusul case that only
 @rhombus(~else) is present, the type of @rhombus(target_expr) is
 unconstrained.

@examples(
  ~eval: eval
  ~defn:
    type Shape
    | circle(radius)
    | rectangle(width, height)
  ~defn:
    fun area(s :: Shape):
      match s
      | circle(r): 3.14*r*r
      | rectangle(w, h): w*h
  ~repl:
    area(circle(1))
    area(rectangle(3, 4))
)

 In the list form of @rhombus(match), the @rhombus([]) and
 @rhombus(cons) clauses can actually be in either order, and a second one
 can be replaced with @rhombus(~else).

@examples(
  ~eval: eval
  ~defn:
    fun sum(lst :: Listof(Number)) :: Number:
      match lst
      | []: 0
      | cons(f, r): f + sum(r)
  ~repl:
    sum([1, 2, 3])
  ~defn:
    fun max_element(lst :: Listof(Number)) :: Number:
      match lst
      | cons(f, r):
          match r
          | []: f
          | ~else:
              block:
                def r_max = max_element(r)
                if f > r_max | f | r_max
      | []: error(#'max_element, "empty list")
  ~repl:
    max_element([1, 20, 3])      
)

 In the syntax-pattern form of @rhombus(match), @rhombus(target_expr)
 must produce a @tech{syntax object}, and it is compared to the quoted
 @rhombus(pattern)s until a match is found. A pattern can include an
 escape with @rhombus($, ~datum) followed by an identifier, in which case it
 binds the identifier to one part of the input syntax or as a
 @tech{repetition} for multiple parts. See @secref("sec:stxobj") for
 more information and for examples.

 In the integer-case form of @rhombus(match), each clause has an integer
 or multiple integers separated by @rhombus(||, ~datum).

@examples(
  ~eval: eval
  ~defn:
    fun describe_quantity(n :: Number):
      match n
      | 0: "none"
      | 1: "single"
      | 2: "pair"
      | 3 || 4 || 5: "some"
      | ~else: "many"
  ~repl:
    describe_quantity(1)
    describe_quantity(4)
)

}

@// ------------------------------------------------------------
@section(~tag: "sec:expr"){Other Expression Forms}

@doc(
  expr.macro 'check:
                $expr
                ~is $expected_expr'
  expr.macro 'check:
                $expr
                ~raises $string_expr'
){

 Checks that @rhombus(expr) produces the same result as
 @rhombus(expected_expr) or raises an exception with an error message
 that contains the string produced by @rhombus($string_expr). Expressions
 @rhombus(expr) and @rhombus(expected_expr) must have the same type,
 while @rhombus($string_expr) must have the type
 @rhombus(String, ~at shplait/type). The result of a @rhombus(check) form
 is @rhombus(#void).

 When the result does not match the expected result, then an error is
 printed, but evaluation continues.

@examples(
  ~eval: eval
  check:
    1+2
    ~is 3
  check:
    1/0
    ~raises "division by zero"
  check:
    1+2
    ~is 4
)

}

@doc(
  ~nonterminal:
    handle_expr: block expr
  expr.macro 'try:
                $expr
                ~catch:
                  $handle_expr'
){

 Returns the same result as @rhombus(expr), unless an exception is
 raised during @rhombus(expr), in which case @rhombus(handle_expr) is
 evaluated and its result is returned. The type of @rhombus(expr) and the
 type of @rhombus(handle_expr) must be the same.

@examples(
  ~eval: eval
  try:
    1+2
    ~catch:
      0
  try:
    1/0
    ~catch:
      0
  try:
    error(#'example, "oops")
    ~catch:
      ["was", "an", "error"]
)

}

@doc(
  expr.macro 'let_cc $id:
                $expr'
){

 Returns the same result as @rhombus(expr), but binds @rhombus(id) to a
 function that represents the @deftech{contination} of @rhombus(let_cc)
 expression. If @rhombus(id) is called, whether before or after
 @rhombus(expr) is returned, the continuation of the call is discarded
 and replaced with the continuation represented by @rhombus(id).

@examples(
  ~eval: eval
  let_cc k:
    1 + k(2)
  10 + (let_cc k:
          1 + k(2))  
)

}

@doc(
  ~nonterminal:
    fun_expr: block expr
  expr.macro 'trace $fun_expr:
                $expr'
){

 Returns the value of @rhombus(expr), but while @rhombus(expr) is being
 evaluated, traces calls and results for the function produced by
 @rhombus(fun_expr). The value of @rhombus(fun_expr) must be a function
 as defined using @rhombus(def) or @rhombus(fun); that is,
 @rhombus(fun_expr) does not have to be an identifier, but it must have
 the same result as the defined identifier.

 Tracing prints @litchar{=>} for each function call, adding one
 additional @litchar{=} to the front of @litchar{=>} for each nesting
 level. It prints @litchar{<=} before showing each result, adding one
 additional @litchar{=} to the end of @litchar{<=} for each nesting
 level. When a traced call has the same continuation as the previous
 traced call, the nesting depth is not increased, and no result is shown
 for the previous call (since it is the same as the new call's result).
 When tracing lazy evaluation (see @secref("sec:lazy")), arguments and
 results may print as @litchar{#<thunk>}, indicating that an expression's
 value has not been demanded, yet.

@examples(
  ~eval: eval
  ~defn:
    fun tail_sum(lst, accum):
      match lst
      | []: accum
      | cons(n, rst): tail_sum(rst, n + accum)
  ~defn:
    fun nontail_sum(lst):
      match lst
      | []: 0
      | cons(n, rst): n + nontail_sum(rst)
  ~repl:
    trace tail_sum:
      tail_sum([1, 2, 3], 0)
    trace nontail_sum:
      nontail_sum([1, 2, 3])
    trace tail_sum:
      nontail_sum([1, 2, 3])                                
)

}

@doc(
  expr.macro '.... $anything ...'
  expr.macro '$expr .... $anything ...'
){

 The @rhombus(....) operator (with four dots) can be used as a prefix,
 infix, or suffix operator. The argument before an infix or suffix
 @rhombus(....) must be a valid expression, but anything after
 @rhombus(....) (in the same shrubbery group) is effectively commented
 out. Furthermore, the overall @rhombus(....) expression can have any
 type.

@examples(
  ~eval: eval
  ~error:
    ....
  ~error:
    .... not sure what should go here! ....
  ~error:
    ....:
      maybe return []?
      maybe call error?
)    

}

@doc(
  expr.macro 'time: $expr'
){

 Returns the resultof @rhombus(expr), but before returning, prints
 information about how time passed when evaluating @rhombus(expr).

}

@doc(
  expr.macro '#%parens ($expr)'
){

 The @rhombus(#%parens) expression form is generally not written out,
 but it is implicitly used whenever a Shplait program uses parentheses
 that are not function-call parentheses.

@examples(
  ~eval: eval
  (1 + 2)
  #%parens (1 + 2)
)

}



@doc(
  expr.macro '#%literal $literal'
){

 The @rhombus(#%literal) expression form is generally not written out,
 but it is implicitly used whenever a Shplait program uses a literal
 value like @rhombus(1), @rhombus(#false), or @rhombus("apple").

@examples(
  ~eval: eval
  1
  #%literal 1
)

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
  expr.macro '$expr mod $expr'
){

 Arithmetic on @rhombus(expr)s of type
 @rhombus(Number, ~at shplait/type), and the overall arithmetic
 expression also has type @rhombus(Number, ~at shplait/type).

 The usual precedence and associativity rules apply, except that
 @rhombus(/) or @rhombus(mod) cannot appear to the right of @rhombus(*).

@examples(
  ~eval: eval
  ~repl:
    1 + 2 * 3 + 8 / 2
    3 mod 2
)

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

 These operators have lower precedence than arithmetic operators.

@examples(
  ~eval: eval
  ~repl:
    1 + 2 < 4
)

}

@doc(
  fun min(n :: Number, m :: Number) :: Number
  fun max(n :: Number, m :: Number) :: Number
  fun floor(n :: Number) :: Number
  fun ceiling(n :: Number) :: Number
){

 Functions that compare and select or round.

@examples(
  ~eval: eval
  min(1, 2)
  max(1, 2)
  floor(1.5)
  ceiling(1.5)
)

}



@doc(
  fun add1(n :: Number) :: Number
  fun sub1(n :: Number) :: Number
){

 Functions that add or subtract @rhombus(1) from a given number.

@examples(
  ~eval: eval
  add1(0)
  sub1(0)
)

}


@doc(
  fun is_even(n :: Number) :: Boolean
  fun is_odd(n :: Number) :: Boolean
){

 Reports when a number is even or odd, respectively.

@examples(
  ~eval: eval
  ~repl:
    is_even(2)
    is_odd(2)
)

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

These operators have lower precedence than arithmetic operators.

@examples(
  ~eval: eval
  ~repl:
    !("apple" == "orange")
)

}


@doc(
  expr.macro '$expr == $expr'
  expr.macro '$expr != $expr'
){

 Compares any two values for (in)equality, as long as the
 @rhombus(expr)s have the same type. The overall comparison expression
 has type @rhombus(Boolean, ~at shplait/type).

 For mutable values, @rhombus(==) checks whether the values are equal
 ``now,'' even though they may be changed to be different in the future
 (and @rhombus(!=) checks that values are not equal now).

@examples(
  ~eval: eval
  ~repl:
    1 == 1
    "apple" == "orange"
  ~repl:
    def b = box(1)
    b == box(1)
    set_box(b, 2)
    b == box(1)
)

}

@doc(
  expr.macro '$expr && $expr'
  expr.macro '$expr || $expr'
){

 Boolean ``and'' and ``or'' in short-circuiting form. That is, if the
 result of the first @rhombus(expr) is @rhombus(#false) for @rhombus(&&)
 or @rhombus(#true) for @rhombus(||), then the second expression is not
 evaluated. Both expressions must have type
 @rhombus(Boolean, ~at schplait/type).

 These operators have lower precedence than all other operators, and
 @rhombus(||) has lower precedence than @rhombus(&&).

@examples(
  ~eval: eval
  ~repl:
    1 == 1  &&  2 == 2
    1 == 2  ||  2 != 2
)

}

@// ------------------------------------------------------------
@subsection(~tag: "sec:void"){Void}

@doc(
  type 'Void'
){

 The type of an expression that has a side effect and produces the value
 @rhombus(#:void). For example, the result type of @rhombus(println) is
 @rhombus(Void, ~at shplait/type).

}

@// ------------------------------------------------------------
@subsection(~tag: "sec:string"){Strings}

@doc(
  type 'String'
){

 The type for expressions that produce strings.

}

@doc(
  fun string_append(str1 :: String, str2 :: String) :: String
){

 Appends two strings.

@examples(
  ~eval: eval
  string_append("apple", "banana")
)

}

@doc(
  fun string_length(str :: String) :: Number
){

 Returns the length of @rhombus(str) in characters.

}


@doc(
  fun to_string(v :: ?a) :: String
){

 Converts any value to a printed form as a string.

@examples(
  ~eval: eval
  to_string(1)
  to_string("apple")
  to_string([1, 2, 3])
  to_string(fun (x): x)
  to_string('fun (x): x')
)

}


@doc(
  expr.macro '$expr +& $expr'
){

 Converts the result of each @rhombus(expr) to a string like
 @rhombus(to_string), then appends the strings like
 @rhombus(string_append).

@examples(
  ~eval: eval
  "a" +& "b"
  "a" +& 1
  1 +& "a"
  "choices are " +& [1, 2, 3]

)

}


@doc(
  fun print(v :: ?a) :: Void
  fun println(v :: ?a) :: Void
){

 Converts @rhombus(v) to a string using @rhombus(to_string), then prints
 the string. The @rhombus(println) function prints a newline after
 printing that string.

@examples(
  println(1 + 2)
  block:
    print("a")
    print("b")
    println("")
)

}


@doc(
  type 'Char'
){

 The type for a character within a string.

}


@doc(
  fun string_get(str :: String, i :: Number) :: Char
){

 Returns the @rhombus(i)th character of a string, counting from
 @rhombus(0).

@examples(
  ~eval: eval
  string_get("apple", 0)
)

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

@doc(
  expr.macro '#' $id'
){

 A literal symbol.

@examples(
  ~eval: eval
  #'apple
  "apple"
)

}

@doc(
  fun string_to_symbol(s :: String) :: Symbol
){

 Converts a string to a symbol that has the same character content. Note
 that @rhombus(to_string) converts a symbol to a string.

@examples(
  ~eval: eval
  string_to_symbol("apple")
)

}

@// ------------------------------------------------------------
@subsection(~tag: "sec:list"){Lists}

A @deftech{list} has a sequence of elements of a uniform type. That is, the elements of a list
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

@examples(
  ~eval: eval
  [1, 2, 3 + 4]
  ["apple", "banana"]
  []
)

}

@doc(
  fun cons(elem :: ?a, lst :: Listof(?a)) :: ?a
  fun first(lst :: Listof(?a)) :: ?a
  fun rest(lst :: Listof(?a)) :: Listof(?a)
  fun is_cons(lst :: Listof(?a)) :: Boolean
  fun is_empty(lst :: Listof(?a)) :: Boolean
){

 The @rhombus(cons) function produces a list given its first element
 plus the rest of the elements already in a list. The @rhombus(first)
 function returns the first element of a nonempty list. The
 @rhombus(rest) funrction returns a list containing all but the first
 element of a nonempty list. The @rhombus(is_cons) and @rhombus(is_empty)
 functions report whether a list is nonempty or empty, respectively.

 The @rhombus(first) and @rhombus(rest) functions raise anexception when
 given an empty list.

@examples(
  ~eval: eval
  cons(1, [2, 3])
  first(["apple", "banana", "coconut"])
  rest(["apple", "banana", "coconut"])
  ~error:
    first([])
)

}

@doc(
  fun length(lst :: Listof(?a)) :: Number
){

 Returns the number of elements in a list.

@examples(
  ~eval: eval
  length(["apple", "banana"])
)

}

@doc(
  fun list_get(lst :: Listof(?a), i :: Number) :: ?a
){

 Returns the @rhombus(i)th element of a list, counting from
 @rhombus(0).

@examples(
  ~eval: eval
  list_get(["apple", "banana"], 1)
)

}


@doc(
  fun append(lst1 :: Listof(?a), lst2 :: Listof(?a)) :: Listof(?a)
){

 Produces a list that has the items of the first given list followed by
 the items of the second given list.

@examples(
  ~eval: eval
  def my_list = [1, 2, 3]
  def my_other_list = [3, 4]
  append(my_list, my_other_list)
  my_list
)

}

@doc(
 fun reverse(lst :: Listof(?a)) :: Listof(?a)
){

 Returns a list that has the same elements as the given one, but in
 reverse order.

@examples(
  ~eval: eval
  reverse([1, 2, 3])
)

}

@doc(
  fun member(elem :: ?a, lst :: Listof(?a)) :: Boolean
){

 Determines whether a value is an item in a list. Items are compared using
 @rhombus(==).

@examples(
  ~eval: eval
  member(2, [1, 2, 3])
  member(4, [1, 2, 3])
)

}

@doc(
  fun map(f :: ?a -> ?b, lst :: Listof(?a)) :: Listof(?b)
){

 Applies a function in order to each element of a list and forms a new
 list with the results.

@examples(
  ~eval: eval
  map(add1, [1, 2, 3])
  map(to_string, [1, 2, 3])
)

}

@doc(
 fun map2(f :: ?a ?b -> ?c, lst1 :: Listof(?a), lst2 :: Listof(?a))
   :: Listof(?c)
){

 Applies a function in order to each pair of elements from two lists in
 ``parallel,'' forming a new list with the results. An exception is raised
 if the two lists have different lengths.

@examples(
  ~eval: eval
  map2(fun (x, y): x +y, [1, 2, 3], [4, 5, 6])
)

}

@doc(
  fun filter(f :: ?a -> Boolean, lst :: Listof(?a)) :: Listof(?a)
){

 Returns a list containing (in order) the items of a given list for which
 a given function returns true.

@examples(
  ~eval: eval
  filter(is_even, [1, 2, 3, 4])
  filter(is_odd, [1, 2, 3, 4])
)

}

@doc(
 fun foldl(f :: (?a, ?b) -> ?b, init :: ?b, lst :: Listof(?a))
   :: ?b
 fun foldr(f :: (?a, ?b) -> ?b, init :: ?b, lst :: Listof(?a))
   :: ?b
){

 Applies a function to an accumulated value and each element of a list,
 each time obtaining a new accumulated value. The second argument to
 @rhombus(foldl) or @rhombus(foldr) is the initial accumulated value, and
 it is provided as the first argument in each call to the given function
 @rhombus(f). While @rhombus(foldl) applies the function or items in the
 list from from to last, @rhombus(foldr) applies the function or items in
 the list from last to first.

@examples(
  ~eval: eval
  foldl(fun (x, y): x+y, 10, [1, 2, 3])
  foldl(fun (n, r): cons(to_string(n), r), [], [1, 2, 3])
  foldr(fun (n, r): cons(to_string(n), r), [], [1, 2, 3])
)

}

@// ------------------------------------------------------------
@subsection(~tag: "sec:array"){Arrays}

Like a @tech{list}, an @deftech{array} has a sequence of elements of a
uniform type. Unlike a list, any element of the array can be accessed in
constant time using square-bracket indexing, as in @rhombus(a[0]).
Further unlike a list, an array is mutable, and an array element can be
changed by commbining square brackets with @rhombus(:=) for assignment,
as in @rhombus(a[0] = 1).

An array is created with @rhombus(Array), such as
@rhombus(Array(1, 2, 3)), or with @rhombus(make_array). Using square
brackets implicitly uses the @rhombus(#%index) form, but
@rhombus(#%index) is normally not written.

@doc(
  type 'Arrayof($type)'
){

 The type of an array whose elements have type @rhombus(type).

}

@doc(
  expr.macro 'Array($expr, ...)'
){

 Produces an array whose elements in order as the values produced by the
 @rhombus(expr)s. All of the @rhombus(expr)s must have the same type.

@examples(
  ~eval: eval
  Array(1, 2, 3 + 4)
)

}

@doc(
  ~nonterminal:
    array_expr: block expr
    index_expr: block expr
    val_expr: block expr
  expr.macro '$array_expr #%index [$index_expr, ...]'
  expr.macro '$array_expr #%index [$index_expr, ...] := $val_expr'
){

 Accesses of updates an array element. Normally, @rhombus(#%index) is
 omitted, since it's implied when using square brackets as an
 array-access form.

@examples(
  ~eval: eval
  def a = Array("a", "b", "c")
  a[0]
  a[0] := "z"
  a[0]
  a
)

}


@doc(
  fun make_array(n :: Number, val :: ?a) :: Arrayof(?a)
){

 Creates an array with @rhombus(n) elements, all initially @rhombus(val).

@examples(
  ~eval: eval
  make_array(3, "a")
)

}


@doc(
  fun array_length(arr :: Arrayof(?a)) :: Number
){

 Returns the number of slots in an array.

@examples(
  ~eval: eval
  array_length(Array("a", "b", "c"))
)

}



@// ------------------------------------------------------------
@subsection(~tag: "sec:box"){Boxes}

A @deftech{box} is a mutable object that holds a single value.

@doc(
  type 'Boxof($type)'
){

 The type for a box that holds a @rhombus(type) value.

}

@doc(
  fun box(val :: ?a) :: Boxof(?a)
  fun unbox(bx :: Boxof(?a)) :: ?a
  fun set_box(bx :: Boxof(?a), val :: ?a) :: Void
){

 The @rhombus(box) function a box that is distinct from all existing
 boxes and that initially holds @rhombus(val). The @rhombus(unbox)
 function extracts the current value of a box, and the @rhombus(set_box)
 function changes the value that is held by a box.

@examples(
  ~eval: eval
  def b = box(1)
  b
  unbox(b)
  set_box(b, 2)
  unbox(b)
  ~error:
    set_box(b, "apple")
)

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
 @rhombus(type). The type of an empty tuple is written as @rhombus(()).

 The @rhombus(*, ~at shplait/type) type operator for tuples has higher
 precedence (i.e., joins more tightly) than the
 @rhombus(->, ~at shplait/type) type operator for functions.

@examples(
  ~eval: eval
  values(1, "apple")
  values()
  values(1, fun (x :: Number): x)
)

}

@doc(
  expr.macro 'values($expr, ...)'
){

 Creates a tuple whose elements are produced by the @rhombus(expr)s.

 In general extract components of a tuple by using @rhombus(def values).

@examples(
  ~eval: eval
  def tup = values(1, "apple", fun (x): x)
  tup
  def values(n, str, id_func) = tup
  str
)

}

@doc(
  fun fst(tup :: ?a * ?b) :: ?a
  fun snd(tup :: ?a * ?b) :: ?b
){

 Convenience functions for 2-element tuples to extract the first or
 second component, respectively.

}

@doc(
  type '#%parens ()'
  type '#%parens ($type)'
){

 The @rhombus(#%parens) type form is generally not written out, but it
 is implicitly used whenever a Shplait program uses parentheses in a type
 position.

 Parentheses are normally used for mere grouping, but emprt parentheses
 form the type of an empty tuple.

}


@// ------------------------------------------------------------
@subsection(~tag: "sec:map"){Maps}

A @deftech{map} (not to be confused with the @rhombus(map) function on
lists), also known as a @deftech{dictionary}, is a mapping from keys to
values. All keys must have the same type, and all values must have the
same type.

A map is written with curly braces @litchar{{}} around comma-separated
key--value pairs, where the key and value are separated by a colon
@litchar{:}, as in @rhombus({ "a": 1, "b": 2 }). Using curly braces
implicitly uses the @rhombus(#%braces) form, but @rhombus(#%braces) is
normally not written.


@doc(
  ~nonterminal:
    key_type: block type
    val_type: block type
  type 'Mapof($key_type, $val_type)'
){


 The type of a @tech{map} whose keys are of type @rhombus(key_type) and
 values (that keys map to) are of type @rhombus(val_type).

}

@doc(
  expr.macro '#%braces { $key_expr: $val_expr, ... }'  
){

 Produces an immutable @tech{map} whose keys are the values produced by
 the @rhombus(key_expr)s mapping each of them to the value produced by
 the corresponding @rhombus(val_expr). All of the @rhombus(key_expr)s
 must have the same type, and all of the @rhombus(val_expr)s must have
 the same type. Normally, @rhombus(#%braces) is omitted, since it's
 implied when using square brackets as an expression form.

 Since the result map is immutable, it works with @rhombus(map_update),
 but not @rhombus(map_set).

@examples(
  ~eval: eval
  { "a": 1, "b": 2 }
)

}


@doc(
  expr.macro 'MutableMap{ $key_expr: $val_expr, ... }'  
){

 Produces a mutable @tech{map} whose keys and values are initially the
 same as the map produced by the same expression without
 @rhombus(MutableMap).

 Since the result map is mutable, it works with @rhombus(map_set), but not
 @rhombus(map_update).

@examples(
  ~eval: eval
  MutableMap{ "a": 1, "b": 2 }
)

}

@doc(
  fun map_get(map :: Mapof(?a, ?b), key :: ?a) :: Optionof(?b)
  fun map_get_k(map :: Mapof(?a, ?b), key :: ?a,
                success_k :: ?a -> ?c,
                fail_k :: () -> ?c) :: ?c
){

 Functions to look up a key in a @tech{map}. Since the key might not be
 mapped to a value, the value cannote be returned directly. Instead,
 @rhombus(map_get) returns @rhombus(some(@rhombus(val, ~var))) if
 @rhombus(key) is mapped to @rhombus(val, ~var) and @rhombus(none()) if
 @rhombus(key) is not mapped to a value.

 The @rhombus(map_get_k) function calls either @rhombus(success_k) or
 @rhombus(fail_k) and returns the result, depending on whether
 @rhombus(key) is mapped. The call @rhombus(map_get(map, key)) is
 equivalent to @rhombus(map_get_k(map, key, some, none)).

@examples(
  ~eval: eval
  def m = { "a": 1, "b": 2 }
  map_get(m, "a")
  map_get(m, "c")
  map_get_k(m, "a", fun(v): v, fun(): 0)
  map_get_k(m, "c", fun(v): v, fun(): 0)
)

}


@doc(
  fun map_update(map :: Mapof(?a, ?b), key :: ?a, val :: ?b) :: Mapof(?a, ?b)
){

 Produces a new map that is like @rhombus(map), but with @rhombus(key)
 mapped to @rhombus(val). The given @rhombus(map) must be immutable.

@examples(
  ~eval: eval
  def m = { "a": 1, "b": 2 }
  map_update(m, "a", 100)
  map_update(m, "c", 3)
  m
)

}

@doc(
  fun map_set(map :: Mapof(?a, ?b), key :: ?a, val :: ?b) :: Void
){

 Changes @rhombus(map) so that @rhombus(key) is mapped to @rhombus(val).
 The given @rhombus(map) must be mutable.

@examples(
  ~eval: eval
  def m = MutableMap{ "a": 1, "b": 2 }
  map_set(m, "a", 100)
  map_set(m, "c", 3)
  m
)

}

@doc(
  fun map_remove(map :: Mapof(?a, ?b), key :: ?a) :: Mapof(?a, ?b)
  fun map_delete(map :: Mapof(?a, ?b), key :: ?a) :: Void
){

 The @rhombus(map_remove) function works only on immutable maps, and it
 returns a new map without a mapping for @rhombus(key) (if there was
 one). The @rhombus(set_delete) function works only on mutable maps, and
 it modifies the map to remove a mapping for @rhombus(key), if there is
 one.

@examples(
  ~eval: eval
  ~repl:
    map_remove({ "a": 1, "b": 2}, "a")
  ~repl:
    def m = MutableMap{ "a": 1, "b": 2 }
    map_delete(m, "a")
    m
)

}


@doc(
  fun map_keys(map :: Mapof(?a, ?b)) :: Listof(?a)
){

 Returns a list of keys mapped by @rhombus(map).

@examples(
  ~eval: eval
  map_keys({ "a": 1, "b": 2})
)

}

@// ------------------------------------------------------------
@section(~tag: "sec:stxobj"){Syntax Objects, Patterns, and Templates}

@margin_note{See @secref("syntax-object") for an overview of syntax
 objects.}

A @deftech{syntax object} is a representation of source code. It is
written as quoted term using single quotes, such as
@rhombus('1 + * 4 f()'). A syntax object is not a string, even though it
uses quote marks. Instead, the usual @tech{shrubbery} rules apply inside
quotes, and shrubbery structure is preserved in a syntax object. That
is, a string records a sequence of characters, but a syntax object
represents structure that might be written in different ways; in
particular, the printed form of a syntax object tends to use
@litchar{«»} shrubbery notation instead of shrubbery notation's
whitespace-sensitive format.

When a syntax object is written using quotes, the @rhombus(#%quotes)
form is used implicitly, similar to the way that square brackets
implicitly use @rhombus(#%brackets).

@examples(
  ~eval: eval
  'apple'
  #%quotes 'apple'
  '1 + 2'
  'fun (x):
     x + 1'
)

An expression written with quotes is more generally a
@deftech{template}, because it can include @rhombus($, ~datum) as an
escape and an ellipsis (written as @litchar{...}) to instantiate a
repetition. @elemtag("stxpat"){Syntax} @deftech{patterns} in
@rhombus(match) are also written with quotes, @rhombus($, ~datum) as an
escape, and an ellipsis to match a repetition.

Rules for @rhombus($, ~datum) and @litchar{...} in patterns:

@nested(~style: #'inset){

 When @rhombus($, ~datum) appears in a pattern, then it must be followed
 by an identifier. In the simple case that the pattern has no ellipses
 (written as @litchar{...}), then the identifier is bound to a non-empty
 sequence of terms from the corresponding part of the input syntax
 object. Sequences are matched greedily, meaning that as many terms are
 matched to an identifier as possible to find a match for the pattern
 overall.

 @examples(
  ~eval: eval
  ~repl:
    match '1 2 3 4'
    | '1 $x 4': x
  ~repl:
    match 'a b c d'
    | '$x $y': [x, y]
 )

 When an ellipsis @litchar{...} appears in a pattern, then it greedily matches 0 or more
 repetitions of the preceding pattern element. If the preceding element
 contains a @rhombus($, ~datum) escape, then the escaped identifier is not bound
 to a single syntax object, but it is instead bound as a
 @deftech{repetition} that holds each matched term. A repetition can only
 be referenced through a corresponding escape in a @tech{template}.
 Ellipses can be nested in a pattern, and a repettion must be used in a
 template with the same amount of nesting as in its pattern.

 @examples(
  ~eval: eval
  ~repl:
    match 'a a a b d'
    | 'a ... b c ... d': "matches"
  ~repl:
    match 'a a a b'
    | '$x ... b': 'matches as $x ...'
  ~repl:
    match '(a 1) (b 2) (c 3)'
    | '($x $y) ...': ['$x ...', '$y ...']
  ~repl:
    match '(a: 1) (b: 2 3 4) (c: 5 6)'
    | '($x: $y ...) ...': ['$x ...', '($y ...) ...']
 )

 When an ellipsis appears by itself in a shrubbery group, then the
 pattern matches 0 or more repetitions of the preceding group. If the
 group is an alternative written with @litchar{|}, then the pattern
 matches 0 or more alternatives.

 @examples(
  ~eval: eval
  ~repl:
    match 'a
           b c
           d'
    | '$x
       ...':
        '{$x, ...}'
  ~repl:
    match 'cases
           | a
           | b c
           | d'
    | 'cases
       | $x
       | ...':
        '{$x, ...}'
 )

}

Rules for @rhombus($, ~datum) and @litchar{...} in templates:

@nested(~style: #'inset){

 When @rhombus($, ~datum) appears in a template, then it must be followed
 by an identifier or an expression that is written as a single term
 (e.g., a parenthesized expression).

 If an identifier is provided, then it can refer to a @tech{repetition}
 that is bound by a syntax-pattern match, as long as the
 @rhombus($, ~datum)-escaped identifier in the template is under a number of
 ellipses that match the repetition binding. Each element of the
 repetition is put in the result syntax object in place of the escape.
 The rules for patterns above show several example templates that sue repetitions.

 If an escape does not refer to a repetition, then it must have an
 expression that produces a syntax object, and it must not be under any
 ellipses. The syntax object replaces the escape in the result syntax
 object.

 @examples(
  ~eval: eval
  '1 $(if #true | '2' | 'oops') 3'
 )

}

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

 See @elemref("stxpat"){above} for informaTion about @rhombus($, ~datum)
 escapes within the quotes for a syntax object.

}

@doc(
  fun syntax_is_number(stx :: Syntax) :: Boolean
  fun syntax_is_boolean(stx :: Syntax) :: Boolean
  fun syntax_is_symbol(stx :: Syntax) :: Boolean
  fun syntax_is_string(stx :: Syntax) :: Boolean
  fun syntax_is_list(stx :: Syntax) :: Boolean
){

 The @rhombus(syntax_is_number) function checks whether a @tech{syntax
  object} has a single term representing a number, returning
 @rhombus(#true) if so and @rhombus(#false) otherwise. Other functions
 check for different kinds of primitive values as representations.

 The @rhombus(syntax_is_list) function checks whether a syntax object
 would match the pattern @rhombus('[$elem ..., ...]'), returning 
 @rhombus(#true) if so and @rhombus(#false) otherwise.

@examples(
  ~eval: eval
  syntax_is_number('9')
  syntax_is_number('apple')
  syntax_is_list('[w, x y, z]')
  syntax_is_list('a b c')
)

}

@doc(
  fun syntax_to_number(stx :: Syntax) :: Number
  fun syntax_to_boolean(stx :: Syntax) :: Boolean
  fun syntax_to_symbol(stx :: Syntax) :: Symbol
  fun syntax_to_string(stx :: Syntax) :: String
  fun syntax_to_list(stx :: Syntax) :: Listof(Syntax)
){

 The @rhombus(syntax_to_number) function extracts the number that a
 syntax object represents, but only if @rhombus(syntax_is_number) would
 return @rhombus(#false); otherwise an exception is raised. Other
 functions similarly extract values from syntax representations.

@examples(
  ~eval: eval
  syntax_to_number('9')
  syntax_to_list('[w, x y, z]')
)

}


@doc(
  fun number_to_syntax(n :: Number) :: Syntax
  fun boolean_to_syntax(bool :: Boolean) :: Syntax
  fun symbol_to_syntax(sym :: Symbol) :: Syntax
  fun string_to_syntax(str :: String) :: Syntax
  fun list_to_syntax(lst :: Listof(Syntax)) :: Syntax
){

 The inverse of @rhombus(syntax_to_number), etc., converting a value
 into asyntax representation.

@examples(
  ~eval: eval
  number_to_syntax(9)
  list_to_syntax(['w', 'x y', 'z'])
)

}


@doc(
  fun syntax_split(stx :: Syntax) :: Listof(Syntax)
  fun syntax_join(stx :: Syntax) :: Listof(Syntax)
){

 Functions for simple splitting and joins tasks. For more general and
 precise tasks, use @rhombus(match) and template construction with
 @rhombus(#%quotes), potentially converting to and from form that
 @rhombus(list_to_syntax) and @rhombus(syntax_to_list) can handle.

 The @rhombus(syntax_split) function takes a single-group syntax oibject
 and splits it into a list of term syntax objects, or it takes a
 multi-group syntax object and splits it into a list of group syntax
 objects.

 The @rhombus(syntax_join) function combines a list of term syntax
 objects into one group syntax object, and it splices a list of
 multi-group syntax objects into a multi-group syntax object.

@examples(
  ~eval: eval
  syntax_split('1 2 3')
  syntax_split('1 2 3
                4 5')
  syntax_join(['1', '2', '3'])
  syntax_join(['1', '2 3', '4'])
  syntax_join(['1', '2 3', '4
                            5'])
)

}


@doc(
  fun syntax_read() :: Syntax
){

 Reads shrubbery input from the program's current input and produces a
 syntax object. Assuming no syntax errors, input is read until terminated
 by an end-of-file.

}

@// ------------------------------------------------------------

@section(~tag: "sec:module"){Modules and Imports}

A Shplait program that starts
@rhombus(#,(hash_lang()) #,(rhombuslangname(shplait))) is a module, and
its definitions are all implicitly exported for use by other modules.
Use @rhombus(import) in a module to import definitions from other
modules. The @rhombus(module) form adds to a submodule, which is nested
inside another module.

@doc(
  defn.macro 'import:
                $import_spec
                ....'

  grammar import_spec:
    $module_path
    #,(@rhombus(open, ~impo)):
      $module_path
      ...

  grammar module_path:
    $id
    $id / $module_path
    lib($module_string)
    file($path_string)

  impo.macro 'open: $module_path; ...'
){

 The @rhombus(import) form acts like a definition for all of the exports
 of another module, making them available in the current definition
 context. Typically, @rhombus(import) is used in an immediate module
 body, but it also can be used in a nested form, such as a
 @rhombus(block) form.

 If @rhombus(open, ~impo) is not used, then the last component of
 @rhombus(module_path) (not counting a file suffix, if any), is used to
 prefix all of the imported names. Use the prefix, then @litchar{.} then
 a name exported from the module to use that name. If
 @rhombus(open, ~impo) is used for the module, then its exported names
 can be used directly, without a prefix.

}

@doc(
  decl.macro 'module $id:
                $definition_or_expr
                ...'
){

 Adds to the submodule name @rhombus(id) within the enclosing module.
 Typically, @rhombus(id) is @rhombus(test, ~datum), which adds to a
 submodule that DrRacket automatically runs after the enclosing module,
 so that tests within the submodule can run after all definitions are
 ready. The submodule name @rhombus(main, ~datum) is also special; it is
 run when the enclosing module is run as a main program, but not when the
 enclosing module is used as imported into another module.

}

@// ------------------------------------------------------------

@section(~tag: "sec:macro"){Macros}

@doc(
  defn.macro '«macro '$id $pattern':
                 '$template'»'
){

 Defines @rhombus(id) as a macro that matches uses of @rhombus(id)
 followed by matches to @rhombus(pattern), expanding to
 @rhombus(template). The @rhombus(pattern) and @rhombus(template) can
 included uses of @rhombus($, ~datum) to bind and reference pattern
 variables. In a template, @rhombus($, ~datum) can only be followed by a pattern
 variable.

@examples(
  ~eval: eval
  ~defn:
    macro 'let $id = $rhs:
             $body':
      'block:
         def tmp = $rhs
         block:
           def $id = tmp
           $body'
  ~repl:
    let x = 1:
      let x = x + 2:
        x
)

}

@// ------------------------------------------------------------

@section(~tag: "sec:inference"){Type Checking and Inference}

Type checking and inference is just as in ML (Hindley-Milner), with
a few small exceptions:

@itemlist(

 @item{Functions can take multiple arguments, instead of requring a @tech{tuple}
   of arguments. Thus, @rhombus((Number, Number) -> Number, ~at shplait/type) is a different type
   than either @rhombus((Number * Number) -> Number, ~at shplait/type), which is the tuple
   variant, or @rhombus(Number -> (Number -> Number), ~at shplait/type), which is the curried
   variant.}

 @item{Since all top-level definitions are in the same
   mutually-recursive scope, the type of a definition's right-hand
   side is not directly unified with references to the defined
   identifier on the right-hand side. Instead, every reference to an
   identifier---even a reference in the identifier's definition---is
   unified with a instantiation of a polymorphic type inferred for the
   definition.

   Compare OCaml:

@verbatim(~indent: 2){
       # let rec f = fun x -> x
             and h = fun y -> f 0 
             and g = fun z -> f "x";;
       This expression has type string but is here used with type int
}

    with

@examples(
  ~eval: eval
  ~defn:
    fun f(x): x
    fun h(y): f(0)
    fun g(y): f("x")
  ~repl:
    f
    h
    g
)

   A minor consequence is that polymorphic recursion (i.e., a self
   call with an argument whose type is different than that for the
   current call) is allowed. Recursive types, however, are prohibited.
   Polymorphic recursion is not decidable, so see @rhombus(~fuel) in
   @secref("sec:fuel").

   The usual value restriction applies for inferring polymorphic
   types, where expression matching the following grammar
   (@emph{before} macro expansion, unfortunately) are considered
   values:

@doc(
  ~nonterminal_key: block
  ~nonterminal:
    maybe_type: fun maybe_type ~defn
  grammar value:
    ($value)
    fun ($typed_id, ...) $maybe_type: $expr
    values($value, ...)
    [$value, ...]
    $variant_id($value, ...)
    $literal
    $id
){}

   where @rhombus(variand_id, ~var) refers to any
   or a constructor bound by @rhombus(type).}

 @item{Since all definitions are recursively bound, and since the
   right-hand side of a definition does not have to be a function, its
   possible to refer to a variable before it is defined. The type
   system does not prevent ``reference to identifier before
   definition'' errors.}

 @item{Interactive evaluation (e.g., in DrRacket's interactions
   window) can redefine identifiers that were previously defined
   interactively or that were defined in a module as mutable.
   Redefinition cannot change the identifier's type. Due to a
   limitation of the type checker, identifiers of polymorphic type
   cannot be redefined or redeclared. Type declarations are allowed in
   interactive evaluation, but a declared type is never treated as a
   polymorphic type.}

)

When typechecking fails, the error messages reports and highlights (in
pink) all of the expressions whose type contributed to the
failure. That's often too much information. As usual, explicit type
annotations can help focus the error message.

Type checking can be disabled for a Shplait module using
@rhombus(~untyped) @tech{language option}. See @secref("sec:untyped")
for more information.

@// ------------------------------------------------------------

@section(~tag: "sec:option"){Language Options}

Keyword @deftech{language options} written immediately after
@rhombus(#,(hash_lang()) #,(@rhombuslangname(shplait))) change the
language that is implemented by
@rhombus(#,(hash_lang()) #,(@rhombuslangname(shplait))). ``Immediately
after'' means that no definitions or expressions can appear before a
keyword but whitespace and comments can precede a keyword.

The @rhombus(~untyped), @rhombus(~lazy) or @rhombus(~accomodating), and
@rhombus(~fuel) modifiers can be combined within a module, each on its
own line, and the combination can be declared in any order. Only one of
@rhombus(~lazy) and @rhombus(~accomodating) can be used.

@subsection(~tag: "sec:untyped"){Untyped Mode}

The @rhombus(~untyped) @tech{language option} disables type
checking. The syntax of a @rhombuslangname(shplait) module is the same
with and without @rhombus(~untyped), but types are ignored when
@rhombus(~untyped) is specified. An untyped Shplait module can
interoperate with a typed Shplait module, but soundness guarantees
normally provided by Shplait are not preserved.

@subsection(~tag: "sec:lazy"){Lazy Mode}

The @rhombus(~lazy) @tech{language option} changes Shplait to
lazy evaluation. The syntax and type system are unchanged, but the
evaluation of certain expression positions are delayed until forced (by
a test or by printing, ultimately):

@itemlist(

 @item{the right-hand side of a definition;}

 @item{argument in a function call (including calls to variant
 constructors);}

 @item{expressions for elements in a list construction using @litchar{[]},
 in a tuple using @rhombus(values), or an array using @rhombus(Array);}

 @item{expressions for values (not keys) in a map construction using
 @litchar{{}}; and}

 @item{the right-hand side of an assignment}

 @item{... but expressions fitting the @nontermref(value) grammar are
 not delayed.}

)

Printing a value forces all components of the value, such as elements of
a list. Note that every module-level expression's result is printed,
unless the result is @rhombus(#void), so that forces results.
Accessing just one component of a compound value (such as an
element of a list) will not force the other components. Names of
predefined functions like @rhombus(map) refer to a lazy variant in a
lazy context or an eager variant in an eager context.

A lazy Shplait module will not interoperate well with an eager module in
general, but use Use @rhombus(~accomodating) in place of @rhombus(~lazy)
to define a Shplait module that uses eager evaluation and can
interoperate with a lazy Shplait module.

@subsection(~tag: "sec:fuel"){Inference Fuel}

Use @rhombus(~fuel #,(@rhombus(amount, ~var))) as a @tech{language option} to
specify how much effort should be spent resolving potentially cyclic
dependencies due to inference of polymorphic recursion. The default fuel
amount is 100.
