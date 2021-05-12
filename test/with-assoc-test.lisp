
(cl:in-package #:with-assoc-test)

(fiveam:in-suite :with-assoc)

(defparameter *al* '((:foo . "bar")
                     (:baz . "wizard")))

(fiveam:test with-assoc-1
  (is (equal
       (with-assoc:with-assoc (foo)
           *al*
         foo)
       "bar")))

(fiveam:test with-assoc-2
  (is (equal
       (with-assoc:with-assoc (:foo)
           *al*
         foo)
       "bar")))

(fiveam:test with-assoc-3
  (is (equalp
       (with-assoc:with-assoc (:foo :baz)
           *al*
         (list foo baz))
       '("bar" "wizard"))))

(fiveam:test with-assoc-4
  (is (equalp
       (with-assoc:with-assoc (foo baz)
           *al*
         (list foo baz))
       '("bar" "wizard"))))

(defparameter *al2* '(("FOO" . "bar")
                      (:baz . "wizard")))

(fiveam:test with-assoc-4
  (is (equalp
       (with-assoc:with-assoc (:foo :baz)
           *al2*
         (list foo baz))
       '(nil "wizard"))))

(fiveam:test with-assoc-5
  (is (equalp
       (with-assoc:with-assoc ("FOO" :baz)
           *al2*
         (list foo baz))
       '("bar" "wizard"))))


(fiveam:test with-assoc-equal-1
  (is (equalp
       (with-assoc:with-assoc-equal ("FOO" :baz)
           *al2*
         (list foo baz))
       '("bar" "wizard"))))

(fiveam:test with-assoc*-1
  (is (equalp
       (with-assoc:with-assoc* ((:foo :baz))
           *al2*
         (list foo baz))
       '(nil "wizard"))))

(fiveam:test with-assoc*-2
  (is (equalp
       (with-assoc:with-assoc* (("FOO" :baz))
           *al2*
         (list foo baz))
       '("bar" "wizard"))))

(fiveam:test with-assoc*-3
  (is (equalp
       (with-assoc:with-assoc* (("FOO" :baz) :test 'equal)
           *al2*
         (list foo baz))
       '("bar" "wizard"))))

(fiveam:test with-assoc*-4
  (is (equalp
       (with-assoc:with-assoc* (("FOO" baz) :test 'equal)
           *al2*
         (list foo baz))
       '("bar" "wizard"))))

(fiveam:test with-assoc*-5
  (is (equalp
       (with-assoc:with-assoc* ((("FOO" foo) :baz) :test 'equal)
           *al2*
         (list foo baz))
       '("bar" "wizard"))))

(fiveam:test with-assoc*-6
  (is (equalp
       (with-assoc:with-assoc* ((("FOO" foo) (:baz bazzy)) :test 'equal)
           *al2*
         (list foo bazzy))
       '("bar" "wizard"))))

;; TODO: test key and test-not args to with-assoc*


