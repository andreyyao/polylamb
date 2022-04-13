## Language Features from SML that are supported:

### Types

#### Base types:

* `int` (64 bit signed)

* `bool` (true or false)

[comment]:<>(TODO `real` (IEEE 64 bit))

#### Type constructors: 

* `t1 * t2` (Tuple)

* `t1 -> t2` (Function type)

### Operations

* Arithmetic: `+`, `-`, `*`, [comment]:<>(`mod`)

* Comparison: `=`, `<`, `>`, `<=`, `>=`, `<>`

[comment]:<>(* Boolean: `not`, `andalso`, `orelse`)

### Keywords

* `if b then e1 else e2` If expressions

* `fn pat => e` Anonymous function

	`pat` is a comma separated variable bindings, surrounded
	by parentheses. Each variable binding needs to be type annotated.
	[comment]:<>(TODO allow actual patterns, i.e. nested tuples and such)

* `rec` Allowing function to self-reference

* `val a : t = e` Binding [comment]:<>(TODO `fun (args..) : t = e`)

* [comment]:<>(`let @bindings@ in e end`)

