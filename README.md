jtl-haskell
===========

Interpreter
--------------------

Start the interpreter by running `./jtli`. Available commands:

* `<expr>` - evaluate given expression;
* `:e[xplain] <expr>` - print AST of the expression;
* `:s[et] [<var>] <expr>` - evaluate the expression and remember the result; if `<var>` is provided (either
  named `@name` or indexed `@42`), the variable with computed value is added to the current environment; otherwise
  new environment is created with computed value as a current document;
* `:l[oad] [<var>] <file>` - similar to `:s`, but read the value from given file;
* `:q[uit]` - exit interpreter.

Examples
--------------------

* `[1, 2, 3, 4, 5].*::count(@ % 2 == 0)`
* `[1, 7, 9, 4, 2 , 3].*::first(@ % 3 == 0)`
* `[[5], [2, 3], [8, 8]].*.*`
