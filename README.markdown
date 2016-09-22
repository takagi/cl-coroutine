# Cl-Coroutine

CL-COROUTINE is a coroutine library for Common Lisp. It uses CL-CONT continuations library in its implementation.

## Example

Coroutines can be defined using `defcoroutine` macro.

To use defined coroutines, first create a coroutine object with calling `make-coroutine` function, then just `funcall` to process it.

`yield` macro control back to the context which called the coroutine and the coroutine will resume processing at this point when it will be called again.

    ;; define a coroutine using DEFCOROUTINE macro
    (defcoroutine example (whom)
      (format t "First greeting to: ~A~%" whom)
      (yield 1)
      (format t "Second greeting to: ~A~%" whom)
      (yield 2)
      (format t "Third greeting to: ~A~%" whom)
      (coexit 3)
      (format t "No greeting to: ~A~%" whom)
      (yield 4))
    => EXAMPLE

    ;; make a coroutine object
    (setf coroutine (make-coroutine 'example))
    => a coroutine object

    ;; funcall it
    (funcall coroutine "Smith")
    >> First greeting to: Smith
    => 1

    ;; funcall again
    (funcall coroutine "Johnson")
    >> Second greeting to: Johnson
    => 2

    ;; funcall again and coexit
    (funcall coroutine "Williams")
    >> Third greeting to: Williams
    => 3

    ;; funcall after coexit just returns no value
    (funcall coroutine "Brown")
    => No value

    ;; you can also use WITH-COROUTINE macro to set up coroutines,
    ;; which provides calling coroutines without explicit FUNCALL
    (with-coroutine (example)
      (example "Smith")
      (example "Johnson"))
    >> First greeting to: Smith
    >> Second greeting to: Johnson
    => 2


## Installation

You can install `cl-coroutine` via Quicklisp:

    (ql:quickload :cl-coroutine)


## Restrictions

CL-COROUTINE has some restrictions because of its dependency on CL-CONT library.
* special forms that CL-CONT library does not support with CALL/CC
* coroutines with very long definition might need much time to compile


## API

### [Macro] defcoroutine

    DEFCOROUTINE coroutine-name arg &body body => coroutine-name

Defines a new coroutine named `coroutine-name` that has atmost one argument as `arg`. The definition of coroutine is stored in the property list of `coroutine-name` symbol. Defined coroutines will be created using `make-coroutine` function.

### [Macro] yield

    YIELD [result] => |

Yields control back to the context which called the coroutine, passing along any multiple values that were passed to it. The coroutine will resume processing at this point when it will be called again. Any arguments passed to the next calling will be set to the coroutine's corresponding parameters implicitly.

### [Macro] coexit

    COEXIT [result] => |

Returns control to the context which called the coroutine, passing along any multiple values that were passed to it. The difference from `yield` macro is that the coroutine will never resume processing at this point anymore. If the coroutine will be called again, it will just return no value.

### [Function] make-coroutine

    MAKE-COROUTINE coroutine-name => coroutine

Creates and returns a coroutine corresponding to `coroutine-name`. The returned coroutine can be called with `funcall` or `apply` functions.

### [Macro] with-coroutine

    WITH-COROUTINE (coroutine-name) &body body => results

`with-coroutine` uses `make-coroutine` to create a coroutine with name `coroutine-name` and defines a local macro with name `coroutine-name` binding it with the coroutine. `with-coroutine` evaluates the body as an implicit progn with the macro.


## Author

* Masayuki Takagi (kamonama@gmail.com)


## Copyright

Copyright (c) 2014 Masayuki Takagi (kamonama@gmail.com)

## License

Licensed under the LLGPL License.
