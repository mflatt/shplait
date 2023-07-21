Shplait is meant to resemble Rhombus—a Shplait program should run with
the `type` macro at the end of this document and other, similar small
modifications—and Shplait is also implemented in Rhombus. Since
Shplait is one of the first implementations of a language using
Rhombus, it demonstrates how language building with Rhombus works.

Glue Code and Library Functions
===============================

To get started, some glue code is needed in Racket for `#lang shplait`
to work. That's in "main.rkt" and "private/core.rkt". The part in
"private/core.rkt" includes module and REPL wrappers to drive the type
checker's post-expansion unification completion and to make the REPL
print out a type before evaluating. Hopefully, more of that can be
implemented in Rhombus in the future, although the file suffix will
have to stay ".rkt".

The "private/core.rkt" serves a language for implement some parts of
Shplait in core Shplait. See "private/lib.rhm", which starts with
`#lang shplait/private/core`. The definitions of "private/lib.rhm"
are reexported by "main.rkt".

Type Checking
=============

Overall, Shplait expression and definition forms are written as
Rhombus expression and definition macros. The Shplait `def`, for
example, is a definition macro that expands to Rhombus's `def`. The
type checking strategy is as follows:

 * As a prmitive expresison form is parsed (in the sense of
   `expr_meta.Parsed`), it adds static information to the parsed form
   to report it's type.

 * When a form with subexpressions is parsed, it looks up
   subexpression types with `statinfo_meta.lookup` and
   registers/checks type relationships with `unify_do`. Types have to
   be represented as syntax (because that's how static information
   works), and so there are `type_to_syntax` and `syntax_to_type`
   conversions in various places.

 * To avoid forcing the parsing of expressions too early, a form can
   create a type variable and expand to a `unify` form with the type
   variable and body expression. When `unify` is parsed later, it
   forces parsing on its subexpression and registers a unification
   with the type variable. For example, `def` expands to use `unify`
   for the right-hand side; that way, the Rhombus parsers and expander
   can see all definitions in a context before expanding
   subexpressions that might use them.

The type and inference system is Hindley-Milner, but with a twist that
complicates this picture: inference of polymorphic types of recursive
functions. That's technically not decidable, but the implementaiton
here uses a heuristic that works well, and that heuristic involves
tracking the nesting level of definitions to infer polymophism only
for type variables that were introduced at the definition's level. The
nesting level is reflected by a compile-time frame that is stored in a
syntax parameter. The `unify` form serves a related secondary role,
which is to introduce a new frame for its body, and then to register
the frame's definition at the point where the body is fully expanded.
That's the `unify_phase_2` part of "private/unify.rhm". The glue code
in "private/core.rkt" includes a call to `finish_current_frame` to
finally infer polymorphic types for all the registered definitions of
a module.

The type-inference engine in "private/type.rhm" is a direct port of
the engine from Plait in Racket. It provides `unify_do` and
`unify_defn_do`, for example, to get unification requests and ones
that specifically register definitions that are subject to inference
of polymorphism. The inference engine has facilties to distinguish
some arrow types from others, because that's tused to cusotmize the
error message more reliably when a (non-)function is misused; that was
probably more important for Racket-style syntax, though, where
students are easily confused by extra parentheses.

To support mutable type variables that have a compile-time identity
spanning a module's expansion, `type_to_syntax` registers a type in a
compile-time table and returns a representative gensym. The
`syntax_to_type` function maps that gensym back to a compile-time type
object. To propagate type information across module boundaries and to
the REPL, gensyms that might remain in the expansion residual are
registered with `record_frame_defn_type`, and after a module's body is
expanded, a compile-time expression is added to the end of the module
to recreate types and reregister them back in the compile-time table
(which is fresh for every module expansion).

Rhombus Binding Spaces
======================

Rhombus binding spaces facilitate two key aspects of Shplait:

 * Type expressions like `Number -> Number` are implemented by binding
   types in a `type_expr` space. There's a little bit of a parsing
   trick needed to handle things like `(Number, Number) -> Number`,
   where `(Number, Number)` is not a valid type expresion by itself,
   but otherwise type-expression parsing is straightforward.

 * Let-based polymorphism relies on a syntactic distinction between
   value forms like `fun` or lists constructions (which values forms
   as elements) versus non-value forms like box allocations. The
   `value` binding space allows composable extension of "value" in
   this sense (e.g., `fun` is bound as a macro in the `value` space,
   while `box` is not).

Syntax Objects
==============

Shplait's syntax-object pattern and template language is a little
different than Rhombus, because the default class of a type variable
is a term`Sequence` instead of `Term`. That simplifies parsing a
Shplait language with Shplait patterns, to some degree. It's
implemented by compiling Shplait patterns and templates to Rhombus
patterns and templates, naturally; that bridge turns out to be one of
the larger pieces, aside from the inference engine, because it
requires a traversal over all the different shrubbery shapes.

Some Rhombus Helpers
====================

Some definitions that might help to run a Shplait as a Rhombus program:


```
defn.macro 'type $id
            | $var($field, ...)
            | ...':
  'class $id():
     nonfinal
   class $var($field, ...):
     extends $id
   ...'

def cons = List.cons
bind.macro 'cons($a, $b)':
  '[$a, & $b]'

def syntax_to_symbol = Syntax.unwrap
def syntax_to_number = Syntax.unwrap

fun syntax_is_symbol(stx :: Syntax):
  stx.unwrap() is_a Symbol
fun syntax_is_number(stx :: Syntax):
  stx.unwrap() is_a Number

```
