curry2julia
===========

This package contains the implementation of a compiler
which translates Curry programs into [Julia](https://julialang.org/)
programs. Hence, the compiler assumes that a
[Julia compiler](https://julialang.org/downloads/) (version 1.4 or higher)
is available via the command `julia` in the load path.


Installation
------------

To install the compiler and pre-compile the system libraries
(currently: only the `Prelude`), run `make`. This installs
the compiler as executable `$HOME/.cpm/jucs`.


Usage
-----

To compile a Curry module `Mod` into a Julia program, run

    > jucs Mod

To compile a Curry module `Mod` and execute the function `main`
(an operation without arguments), run

    > jucs -x Mod


Options
-------

Standard options can be passed as command-line arguments or
by setting the environment variable `CJOPTIONS`.
For instance,

    export CJOPTIONS="-x --bfs"

always executes the compiled program with a breadth-first search strategy.

Option `--main`:
  ~ The main function to be executed.
    This must be an operation without arguments.
    If this option is given, the compiler also generates a shell script
    `MODULE.run` to evaluate the main function by invoking Julia
    with the correct load path defined.

Option `--execute`:
  ~ Executes the generated Julia program by evaluating
    the function specified by option `--main` (an operation without arguments).
    If the option `--main` is not provided, the function `main` is executed.

Option `--time`:
  ~ Shows the elapsed time used to execute the main function
    (with the Julia `@time` macro). Since Julia has no explicit
    compilation phase, the elapsed time also contains the compilation time.
    In order to get the pure execution time, one provide an integer
    argument `n` to this option. In this case, the main function
    will be executed `n` times after its first execution and the
    average execution time will be printed.

To see a list of all options, execute

    > jucs --help


Stand-alone execution
---------------------

In order to execute a compiled program directly, one has to set
the Julia load path to the appropriate directories:

    export JULIA_LOAD_PATH=<PKGDIR>/lib:<PKGDIR>/include

If the option `--main` is provided, the compiler generates a shell script
to invoke Julia with the correct load path.


Package contents
----------------

include:
  ~ Various run-time systems for translated Curry programs

lib...:
  ~ Translated standard libraries

src:
  ~ The source code of the compiler