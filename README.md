curry2julia
===========

This [Curry](http://www.curry-lang.org)
package contains the implementation of a compiler
which translates Curry programs into [Julia](https://julialang.org/)
programs. Hence, the compiler assumes that a
[Julia compiler](https://julialang.org/downloads/) (version 1.4 or higher)
is available via the command `julia` in the load path.


Installation
------------

To install the compiler and pre-compile the system libraries
(currently: only the `Prelude`), you need an installed
[Curry](http://www.curry-lang.org) system and the executable of the
[Curry Package Manager](http://www.curry-lang.org/tools/cpm) `cypm`
in your path (otherwise adapt the `Makefile`).
Then simply run

    > make

This installs the compiler as executable `$HOME/.cpm/bin/jucs`.
Therefore, you should include the directory `$HOME/.cpm/bin`
into your path for convenient usage of the compiler.


Usage
-----

To compile a Curry module `Mod` into a Julia program, run

    > jucs Mod

To compile a Curry module `Mod` and execute the function `main`
(which must be declared in `Mod` as an operation without arguments), run

    > jucs -x Mod


Options
-------

There are various options which can be passed as command-line arguments
to the compiler.

To see a **list of all options**, execute

    > jucs --help

Some important options are:

#### `--main`

After compiling the Curry module, execute the function `main`.
This must be an operation without arguments (and without a class context).
One can also use "`--main=<f>`" to execute the operation with name `<f>`.

#### `--execute`

Executes the generated Julia program by evaluating
the function specified by option `--main` (an operation without arguments).
If the option `--main` is not provided, the function `main` is executed.

#### `--dfs`

Use a depth-first search strategy (currently, this is the default).

#### `--bfs`

Use a breadth-first search strategy.

#### `--first`

Terminate the execution after computing a first value.

#### `--interactive`

Use interactive execution, i.e., ask for more values after
printing a value.

#### `--time`

Shows the elapsed time used to execute the main function
(with the Julia `@time` macro). Since Julia has no explicit
compilation phase, the elapsed time also contains the compilation time.
In order to get the pure execution time, one should provide an integer
argument (`--time=<n>`) to this option. In this case, the main function
will be executed `<n>` times after its first execution and the
average execution time will be printed.

#### `--standalone`

With this option, the compiler also generates a shell script
`<Module>.sh` which invokes the compiled Julia program
with the correct load path defined (see also below).


#### Setting standard options

By defining the environment variable `CJOPTIONS`,
one can also set standard options which are always used
For instance, by setting

    > export CJOPTIONS="-x --bfs"

the compiled programs are always executed with a breadth-first search strategy.



Stand-alone execution
---------------------

In order to execute a compiled program directly, one has to set
the Julia load path to the appropriate directories
(where `<PKGDIR>` is the installation directory of this package):

    > export JULIA_LOAD_PATH=<PKGDIR>/lib:<PKGDIR>/include

If the option `--standalone` is provided,
the compiler generates a shell script
to invoke Julia with the correct load path.


Package contents
----------------

* `benchmarks`: Benchmarks to evaluate the system
* `examples`:   Example programs
* `include`:    Various run-time systems for translated Curry programs
* `lib...`:     Translated standard libraries
* `src`:        The source code of the compiler
