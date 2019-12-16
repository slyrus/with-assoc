# with-assoc
A library for working with association lists providing macros that
hide the use of cl:assoc.

## Overview
When working with assoc functions, one ends up calling something like:

```lisp
(cdr (assoc foo :bar)
```

quite frequently. In order to save typing a few characters, we can
define some macro-ology.

If we define an association list such as:

```lisp
(defparameter *al* '((:foo . "bar")
                     (:baz . "wizard")))
```

The standrad way to get access to the element whose car is :foo would be:
```lisp
(cdr (assoc :foo *al*))
```

With the with-assoc library we can do the following:

```lisp
(with-assoc:with-assoc (foo)
    *al*
  foo)
```

Obviously this doesn't save us much typing (quite the opposite in
fact), but if we're using lots of pairs in association lists and
particularly if we're reusing the same values multiple value times, it
can be convenient to have some short-hand syntax for

```lisp
(let ((foo (cdr (assoc *al*) :foo)))
  ...)
```

## common-lisp:assoc
The common lisp assoc function takes an item and a list of pairs
(conses) and returns the first pair whose car satisfies a test. The
specifics of the test are specified by the key and test keyword
arguments. If the key argument is nil (the default) then the car of
the pair itself is tested, otherwise the results of calling (key (car
<pair>)) are tested against item via the provided for test function,
or is via #'eql if no test function is supplied.


## with-assoc
with-assoc is meant to be used when item is tested against the items
in the associative list with #'eql.

```lisp
(with-assoc:with-assoc (foo)
    *al*
  foo)
```

## with-assoc-equal
with-assoc is meant to be used when item is tested against the items
in the associative list with #'equal, such as when the items are
strings, rather than keywords.

```lisp
(defparameter *al2* '(("FOO" . "bar")
                      (:baz . "wizard")))

(with-assoc:with-assoc (:foo :baz)
    *al2*
  (list foo baz))
```

## with-assoc*
with-assoc* take keyword arguments key and test which provide the full
flexibility of the assoc (except for the deprecated :test-not
argument).

```lisp
(with-assoc:with-assoc* ((("FOO" foo) :baz) :test 'equal)
    *al2*
  (list foo baz))
```

