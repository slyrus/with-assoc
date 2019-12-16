
(cl:defpackage #:with-assoc-test
  (:use :cl))

(cl:in-package #:with-assoc-test)

(defparameter *al* '((:foo . "bar")
                     (:baz . "wizard")))

(cdr (assoc :foo *al*))

(with-assoc:with-assoc (foo)
    *al*
  foo)

(with-assoc:with-assoc (:foo)
    *al*
  foo)

(with-assoc:with-assoc (:foo :baz)
    *al*
  (list foo baz))

(with-assoc:with-assoc (foo baz)
    *al*
  (list foo baz))

(defparameter *al2* '(("FOO" . "bar")
                      (:baz . "wizard")))

(with-assoc:with-assoc (:foo :baz)
    *al2*
  (list foo baz))

(with-assoc:with-assoc ("FOO" :baz)
    *al2*
  (list foo baz))

(with-assoc:with-assoc-equal ("FOO" :baz)
    *al2*
  (list foo baz))

(with-assoc:with-assoc* ((:foo :baz))
    *al2*
  (list foo baz))

(with-assoc:with-assoc* (("FOO" :baz))
    *al2*
  (list foo baz))

(with-assoc:with-assoc* (("FOO" :baz) :test 'equal)
    *al2*
  (list foo baz))

(with-assoc:with-assoc* (("FOO" baz) :test 'equal)
    *al2*
  (list foo baz))

(with-assoc:with-assoc* ((("FOO" foo) :baz) :test 'equal)
    *al2*
  (list foo baz))

(with-assoc:with-assoc* ((("FOO" foo) (:baz bazzy)) :test 'equal)
    *al2*
  (list foo bazzy))

;; TODO: test key and test-not args to with-assoc*

